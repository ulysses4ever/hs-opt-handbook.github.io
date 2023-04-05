.. _klister:

..
   Local Variables
.. |klister| replace:: `klister <https://github.com/gelisam/klister/>`__
.. |MegaParsec| replace:: `MegaParsec <https://hackage.haskell.org/package/megaparsec>`__

`Klister and the Ever Growing Cache`
====================================

This chapter is a case study on the first pass of performance engineering for
the |klister| programming language. This case study should be exemplary of any
system which is shortlived, has distinct phases of input and output, and
maintains state. To diagnose the performance issues this case study uses
:ref:`Heap Profiling <Heap Profiling Chapter>` with :ref:`Eventlog <EventLog
Chapter>`, :ref:`Info Table Profiling <IPE Chapter>`, and ... TODO

Introduction to the System
--------------------------

The system we are analyzing is the interpreter for |Klister|. Klister is an
interpreted scheme-like programming language. The exact kind of language is not
particularly relevant; one can expect it to have typical scheme features such as
s-expression syntax, macros and lexical scoping. All we need to know about that
the klister interpreter inputs a program, parses the program, maintains mutable
stores of variables and outputs a result.

To begin performance engineering we'll review the sub-systems that compose the
interpreter. In klister, there are 4 major sub-system:

#. Parser: The system that lexes and tokenizes. Klister employs |MegaParsec| for
   its parser.
#. Expander: Klister is a scheme-like language with hygenic macros. This
   sub-system performs the macro expansion and elaboration typical to these
   kinds of programming languages.
#. Type Checker: Klister is a pure functional language with Hindley-Milner type
   checking and higher-ordered types, including type safe macros. This subsystem
   type checks klister code.
#. Evaluator: The evaluator inputs an abstract syntax tree which represents a
   klister program, executes program in the Haskell runtime system and
   outputs the result.


Characterizing the Problem
--------------------------

The goal is to speed up the klister interpreter, but this goal is too vague to
begin working. We first need to know specifics so that we can make statements
that are actionable and precise. That is, we need to have a reproducible test so
that we can observe where time is spent, and where memory is allocated and
consumed. Then we can correlate these costs to particular sub-systems in the
interpreter. For example, we would like to say "The parser finishes a 100 line
program in 100ms and runs in 100Kb of constant space, but the expander allocates
2Gb and finishes its computation in 2 minutes!". Once we can make precise
statements, we can begin forming hypotheses to speed up the system.

We'll use klister's testsuite as our reproducible test. This will provide a good
sample of programs to test and will allow us to find degenerate programs. Then
to correlate costs to each sub-system we'll profile and lookup the offending
functions in the source code.

.. note::

  A quick note on these degenerate programs and a mental model that you might
  find useful. In the abstract, we can think about the *system space* of the
  system. The system space is the space of all possible semantically equivalent
  systems, for some input and for some available resources. For example, for
  klister, given the input program ``(+ 2 2)`` we have an infinite space of
  possible klister interpreters, some with many machine resources and some with
  few. Similarly, given the same machine to run the klister interpreter, and
  the same input program, we still have an infinite space of klister
  interpreters, some of which will be very fast (in wall time) and some will be
  very slow, depending on their implementation.

  Now we can imagine that there are boundaries in this system space that
  delineate acceptable performance from unacceptable performance, however one
  defines performance and what is acceptable. These degenerate programs are
  inputs *that point to such a boundary*, and thus they are useful allies in
  our performance work. As we improve the interpreter these boundaries will
  change, the space of acceptability will grow and we'll have to find even more
  degenerate programs to observe the boundaries again.

Performance Minded Code Review
------------------------------

Before running the testsuite, we'll begin with a code review to better
understand the interpreter and to read through the code with a performance
engineering mindset. Here is a list of things that are noticeable from reading
through the source code.

... TODO ... add references to the other chapters

#. Lazy Data.Map in the interpreter state.
#. Use of lazy tuples throughout the code base.
#. Lazy modifyIORef in the interpreter state.
#. Lists used as general purpose containers.
#. Lazy State Monad especially in the Expander.
#. Use of Monad Transformers and ExceptT in the interpreter state.
#. Lack of Specialization, especially in ``IORef`` utilities module and in lens
   driven code.
#. Some modules do not use explicit exports.
#. No use of ``-funbox-strict-fields`` for bang patterns or ``UNPACK`` pragmas.
#. using ``foldr`` on a list (see ``Expander.getExports``) that doesn't exploit laziness.
#. using ``Integer`` instead of ``Int``.

We won't being going through each in detail, but instead just highlight the
worst items.

Data.Map With Expensive Keys
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``ExpanderState`` is a key type in the klister interpreter. It is stored in
an ``IORef`` and maintains state for the expander. From a performance
perspective the ``ExpanderState`` is suspect because it employs data structures
that are lazy and yet will eventually use everything in the state. Here is the
definition of the ``ExpanderState``:

.. code-block:: haskell

   data ExpanderState = ExpanderState
  { _expanderWorld :: !(World Value)
  , _expanderNextScopeNum :: !Int
  , _expanderGlobalBindingTable :: !BindingTable
  , _expanderExpansionEnv :: !ExpansionEnv
  , _expanderTasks :: [(TaskID, ExpanderLocal, ExpanderTask)]
  , _expanderOriginLocations :: !(Map.Map SplitCorePtr SrcLoc)
  , _expanderCompletedCore :: !(Map.Map SplitCorePtr (CoreF TypePatternPtr PatternPtr SplitCorePtr))
  , _expanderCompletedPatterns :: !(Map.Map PatternPtr (ConstructorPatternF PatternPtr))
  , _expanderCompletedTypePatterns :: !(Map.Map TypePatternPtr TypePattern)
  , _expanderPatternBinders :: !(Map.Map PatternPtr (Either [PatternPtr] (Scope, Ident, Var, SchemePtr)))
  , _expanderTypePatternBinders :: !(Map.Map TypePatternPtr [(Scope, Ident, Var, SchemePtr)])
  , _expanderCompletedTypes :: !(Map.Map SplitTypePtr (TyF SplitTypePtr))
  , _expanderCompletedDeclTrees :: !(Map.Map DeclTreePtr (DeclTreeF DeclPtr DeclTreePtr))
  , _expanderCompletedDecls :: !(Map.Map DeclPtr (Decl SplitTypePtr SchemePtr DeclTreePtr SplitCorePtr))
  , _expanderModuleTop :: !(Maybe DeclTreePtr)
  , _expanderModuleImports :: !Imports
  , _expanderModuleExports :: !Exports
  , _expanderPhaseRoots :: !(Map Phase Scope)
  , _expanderModuleRoots :: !(Map ModuleName Scope)
  , _expanderKernelBindings :: !BindingTable
  , _expanderKernelExports :: !Exports
  , _expanderKernelDatatypes :: !(Map Datatype DatatypeInfo)
  , _expanderKernelConstructors :: !(Map Constructor (ConstructorInfo Ty))
  , _expanderKernelValues :: !(Env Var (SchemePtr, Value))
  , _expanderDeclOutputScopes :: !(Map DeclOutputScopesPtr ScopeSet)
  , _expanderCurrentEnvs :: !(Map Phase (Env Var Value))
  , _expanderCurrentTransformerEnvs :: !(Map Phase (Env MacroVar Value))
  , _expanderCurrentDatatypes :: !(Map Phase (Map Datatype DatatypeInfo))
  , _expanderCurrentConstructors :: !(Map Phase (Map Constructor (ConstructorInfo Ty)))
  , _expanderCurrentBindingTable :: !BindingTable
  , _expanderExpressionTypes :: !(Map SplitCorePtr Ty)
  , _expanderCompletedSchemes :: !(Map SchemePtr (Scheme Ty))
  , _expanderTypeStore :: !(TypeStore Ty)
  , _expanderKindStore :: !KindStore
  , _expanderDefTypes :: !(TypeContext Var SchemePtr) -- ^ Module-level definitions
  }

These ``Maps`` are suspicious because every type suffixed with ``Ptr`` is a
newtype over an ``Integer``. For example, ``SplitCorePtr`` is an newtype over a
``Unique`` which is itself a newtype over ``Integer``:

.. code-block:: haskell

   newtype SplitCorePtr = SplitCorePtr Unique
     deriving (Eq, Ord)

.. code-block:: haskell

   newtype Unique = Unique Integer
     deriving (Data, Eq, Ord)

This means that ``klister`` has a lot of ``Maps`` effectively indexed on
``Integer``. Clearly an ``IntMap`` would be a better choice of data structure.
An ``IntMap`` has several advantages over ``Data.Map``. ``Data.Map`` is an
implementation of *size balanced binary trees* (see the `hackage
<https://hackage.haskell.org/package/containers-0.6.7/docs/Data-Map.html>`_
docs). The trees require re-balancing in order to maintain the balanced
invariant. This balancing slows down writes (that is, adds work to
``Data.Map.insert``) and consequently merges, because a merge of any two trees
may invoke a rebalancing. In contrast, ``IntMap`` is a big-endian PATRICIA Trie
which never require balancing. Compared to ``Data.Map``, an ``IntMap`` provides
faster writes at the cost of slightly slower reads. Additionally, ``IntMap``
uses less total memory than ``Data.Map``. See Chris Done's comparisons `here
<https://github.com/haskell-perf/dictionaries>`_.

The Klister also uses many ``Map``'s indexed over ``String`` like types. For
example, ``_expanderKernelDatatypes`` is a ``Map Datatype DatatypeInfo``, where
``DataType`` is a record:

.. code-block:: haskell

   data Datatype
     = Datatype
       { _datatypeModule :: !ModuleName -- ^ The module that defines the datatype
       , _datatypeName :: !DatatypeName -- ^ The unique name for the datatype at this module and phase
       }
     deriving (Data, Eq, Ord, Show)


where ``ModuleName`` and ``DatatypeName`` are a ``String`` and ``Text``
respectively:

.. code-block:: haskell

   data ModuleName = ModuleName FilePath | KernelName KernelName
     deriving (Data, Eq, Ord, Show)

   newtype DatatypeName = DatatypeName { _datatypeNameText :: Text }
     deriving newtype (Eq, IsString, Ord, Show, Hashable)
     deriving stock Data

Indexing over ``Data.Map`` over ``String`` like types is a performance
anti-pattern because the ``Ord`` and ``Eq`` instance on ``String`` will need to
check the entire ``String`` in the worst case.

A better datastructure for maps indexed over ``String`` like types is
``Data.HashMap`` from the ``unordered-containers`` library. These maps are
*Hashed Array Mapped Trie's*, so they index over a unique ``hash`` which
represents the key type. These data structures are efficient for any key type
where equality could be expensive; such as ``String``, ``Text`` or other
algebraic data types.

Lazy Tuples
^^^^^^^^^^^

Consider this field of the ``ExpanderState``:

.. code-block:: haskell

  ...
  , _expanderTypePatternBinders :: !(Map.Map TypePatternPtr [(Scope, Ident, Var, SchemePtr)])
  ...

The elements of this map is a list of 4-tuples. This list will have an excessive
amount of indirection to its values. Not only will the interpreter be chasing
pointers in the elements of the list but it'll have to chase pointers for each
element of the tuple. This will be slow if these lists ever become large (over
~30 elements) and if this list will be the subject of folds. For this specific
datatype, there is one fold in the klister interpreter:

.. code-block:: haskell

   else do
     varInfo <- view (expanderTypePatternBinders . at patPtr) <$> getState
     case varInfo of
       Nothing -> throwError $ InternalError "Type pattern info not added"
       Just vars -> do
         p <- currentPhase
         let rhs' = foldr (addScope p) stx
                      [ sc'
                      | (sc', _, _, _) <- vars
                      ]
         withLocalVarTypes
           [ (var, varStx, t)
           | (_sc, varStx, var, t) <- vars
           ] $
           expandOneExpression ty dest rhs'

The code projects ``expanderTypePatternBinders`` and looks up the list that
``patPtr`` points to. It then iterates over that *same* list twice: First, to
project the ``sc'`` from the first position and pass it to ``addScope``. Second,
to project the second, third and fourth positions into a list of 3-tuples and
pass that to ``withLocalVarTypes``. This code can be improved with :term:`Loop
Fusion` to iterate over the list once, using ``foldl'`` instead of ``foldr``,
and by defining a datatype which unpacks every field instead of using ``(,,,)``.

Generally types such as ``(,,,)`` are a path of least resistance when writing
new code in a code base. They are easy to reach for, easy to write and don't
require more domain modeling. However, tuples and especially tuples with more
than two fields are a consistent source of memory leaks. So one is almost always
better off defining a datatype instead of using a tuple for performance.

.. note::

   Of course, one may not want to add yet another datatype to the
   implementation. One may want the datatypes in the implementation to map
   cleanly to domain objects. This a classic tradeoff between performance,
   readability and maintainability.

Running the testsuite
^^^^^^^^^^^^^^^^^^^^^

Klister does not have a benchmark suite, but does have a testsuite (with 124
tests) written in :ref:`tasty <Tasty Chapter>` which outputs the wall time of
each test. So we can compare this test to every other test that reports a time:


.. code-block:: bash

   Test suite klister-tests: RUNNING...
   All tests
     Expander tests
       ...
       Module tests
         Expected to succeed
           ...
           examples/lang.kl:                              OK (0.04s)
           examples/import.kl:                            OK (0.02s)
           examples/macro-body-shift.kl:                  OK (0.03s)
           examples/test-quasiquote.kl:                   OK (0.05s)
           examples/quasiquote-syntax-test.kl:            OK (0.04s)
           examples/hygiene.kl:                           OK (0.84s)
           examples/defun-test.kl:                        OK (0.01s)
           examples/fun-exports-test.kl:                  OK (0.04s)
     Golden tests
       test-quasiquote:                                   OK (0.03s)
       io:                                                OK (0.03s)
       defun-test:                                        OK (0.04s)
       contract:                                          OK (0.11s)
       int-ops:                                           OK (0.03s)
       implicit-conversion:                               OK (7.02s)
       ...
       implicit-conversion-test:                          OK (9.89s)
       higher-kinded-patterns:                            OK (1.80s)
       custom-literals-test:                              OK (0.46s)
       double-define:                                     OK (0.34s)
       custom-module-test:                                OK (0.55s)
       which-problem:                                     OK (0.82s)
       incorrect-context:                                 OK (0.03s)
       bound-vs-free:                                     OK (0.31s)
       meta-macro:                                        OK (0.11s)
       integer-syntax:                                    OK (0.04s)
       import:                                            OK (0.04s)

Notice that both ``implicit-conversion`` and ``implicit-conversion-test`` are
outliers, passing in 7 and 9 *seconds*, whereas each other test passes in well
under a second (except ``higher-kinded-patterns``). Clearly there is room for
improvement.


Restate the Problem
-------------------

We have identified several problems and have a viable test case. For the rest of
the case study we'll focus on speeding up ``implicit-conversion-test`` under the
assumption that our changes will also speed up the other tests and consequently
the entire interpreter. Now we want to get specific and make sure that the
problems we have found are indeed problems for that test case. If the maps are a
problematic factor then we should expect a lot of allocations to come from
``Data.Map.insert``, ``(==)``, ``Ord`` instances and the functions
``Data.Map.Internal.balanceR`` and ``Data.Map.Internal.balanceL``. This is a
good opportunity to :ref:`not think and look <Don't think look>` with a
:ref:`tickyticky <Ticky Chapter>` chapter.

First we'll generate a ticky report for the entire testsuite:

.. code-block:: bash

   12:29:21 ❯ cabal test --test-show-details=streaming --test-options='+RTS -rticky -RTS' --ghc-options='-rtsopts -ticky -ticky-allocd -ticky-dyn-thunk'
     Build profile: -w ghc-9.2.4 -O1
     ...

and check the results sorted by allocations. As a reminder, the column on the
right hand side names the closure the ticky profile is describing, the first
column is entries, the second column is the number of bytes allocated *by* the
code for the closure, the third is the number of bytes allocated *of* each
closure:

.. code-block::

   ~/programming/klister main*
   14:45:37 ❯ cat ticky | tail -n +20 | sort -k2 -nr | less

   53739709 4299176720          0   3 +.>                  ScopeSet.$wallScopeSets'{v rNAX} (fun)
   60292448 3858716672 2149588360   3 +..                  sat_sOYl{v} (ScopeSet) (fun) in rNAX
   81547057 1368797696          0   4 SISM                 ScopeSet.$w$sgo4{v rNAW} (fun)
   57730804 1305110352          0   4 SISM                 ScopeSet.$w$sgo1{v rNAV} (fun)
   61143424  841913088          0   2 SM                   ScopeSet.isSubsetOf_go15{v rOUK} (fun)
    7819243  815587232          0   6 >pii.M               Binding.$w$sgo3{v r1syq} (fun)
   17961626  421056776          0   3 >MM                  Binding.$fMonoidBindingTable_$sunionWith{v r1syc} (fun)
     867831  366262720          0  10 piiSiSL>>>           Parser.Common.$wg{v rk} (fun)
     886661  333384536          0   6 SS>>>>               located{v r1b6H} (Parser) (fun)
    4552387  298031744          0   3 ISM                  Expander.$w$sgo4{v r5BKT} (fun)
    4843152  270145008     612288   1 M                    go15{v s1szA} (Binding) (fun) in r1syd
    2699373  259139808          0   4 >SSM                 Syntax.$w$cmapScopes{v rTEZ} (fun)
   18445979  240603872          0   4 piiM                 Binding.$w$sgo1{v r1syi} (fun)
    1351616  237884416     612288   1 T                    f{v s1szf} (Binding) (fun) in r1syd
    1862523  211065056          0   3 S>M                  ScopeSet.$satKeyIdentity_$sgo15{v rOUv} (fun)
    3383994  186416288   43447360   2 LM                   go15{v sP96} (ScopeSet) (fun) in rOUk
     101588  145802400          0   4 MSSM                 $wexpandOneForm{v r5IwM} (Expander) (fun)
    2607448  125157504          0   2 >S                   Syntax.$fHasScopesSyntax_$cmapScopes{v rTEY} (fun)
   ...

There are several interesting aspects to this ticky profile snippet. First, the
most allocating code is ``ScopeSet.allScopeSets``, it is allocating a dictionary
(``+``) of some type (``.``) and function (``>``). The second most allocating
code is a SAT'd function ``sat_sOYl``, from its description: ``{v} (ScopeSet)
(fun) in rNAX`` we can see that it is a non-exported name (``{v}``), in the
``(ScopeSet)`` module, it is a function ``(fun)`` and is a local function in the
``rNAX`` closure, which is the STG name of the closure for ``allScopeSets`` as
shown in description for ``allScopeSets``.

We also see that the 5th and 6th most allocating functions called are
``ScopeSet.isSubsetOf`` and ``Binding.$fMonoidBindingTable_$unionWith``. That
suggests peculiar usage pattern; ``isSubsetOf`` should only return a ``Bool``
which should not be an allocating function call. ``unionWith`` should be
allocating, but that this occurs in the ``Monoid Binding`` instance means that
the ``Binding Monoid`` instance is heavily allocating. Let's check these
functions in the source code:

.. code-block:: haskell

   data ScopeSet = ScopeSet
     { _universalScopes :: Set Scope
     , _phaseScopes :: Map Phase (Set Scope)
     }


   data Scope = Scope { scopeNum :: Int, scopePurpose :: Text }
     deriving (Data, Eq, Ord, Show)

   newtype Phase = Phase { phaseNum :: Natural }
     deriving (Data, Eq, Ord, Show)

   isSubsetOf :: Phase -> ScopeSet -> ScopeSet -> Bool
   isSubsetOf p scs1 scs2 =
     Set.isSubsetOf (scopes p scs1) (scopes p scs2)


   scopes :: Phase -> ScopeSet -> Set Scope
   scopes p scs = view universalScopes scs `Set.union`
                  view (phaseScopes . at p . non Set.empty) scs


We see that a ``ScopeSet`` is a record of ``Data.Set Scope`` and ``Data.Map``
indexed by ``Phase`` that holds ``Set Scope``. Furthermore, both ``Scope`` and
``Phase`` are Integer-like. So we have an implementation that could use
``IntMap`` and ``IntSet`` instead of ``Data.Map`` and ``Data.Set``.

We know that ``isSubsetOf`` does a lot of allocation. Now we can see where this
allocation is happening. ``isSubsetOf`` checks that ``scs1`` is a subset of
``scs2`` by calling ``Set.isSubsetOf`` on the result of the ``scopes`` function.
``scopes`` is allocating a new ``Set Scope`` from the ``ScopeSet`` via
``Set.union`` using the results of a lookup on the ``phaseScopes`` ``Map``, then
merging two ``Set``'s just to check the subset.

There are several ways to improve the memory performance of this function.
First, we can employ better data structures. We know that this code is
performing a lot of merges, so we should expect an improvement in both time and
memory performance by using an ``IntMap`` and ``IntSet`` because these data
structures provide more efficient merges than ``Data.Set`` and ``Data.Map``.
Second, we can use a better algorithm. From the ticky ``isSubSetOf`` was called
61143424 times. As written this code will perform its lookups and unions each
time, even if we have a duplicate call. So this seems to be a good candidate for
memoization or caching the calls to ``isSubsetOf``. We could also change the
memory layout to avoid building the intermediate ``Set`` in the ``scopes``
function.

The second interesting function was ``unionWith`` in the ``Monoid Binding``
instance. Here is the source code:

.. code-block:: haskell

   newtype BindingTable = BindingTable { _bindings :: Map Text [(ScopeSet, Binding, BindingInfo SrcLoc)] }
     deriving (Data, Show)

   instance Semigroup BindingTable where
     b1 <> b2 = BindingTable $ Map.unionWith (<>) (view bindings b1) (view bindings b2)

   instance Monoid BindingTable where
     mempty = BindingTable Map.empty

A ``BindingTable`` is a ``Map`` keyed on ``Text`` that holds a list of triples
and the ``Semigroup`` instance is the origin of the ``unionWith`` in the ticky
profile. This type is likely too lazy. ``Data.Map`` keyed on ``Text`` relies on
the ``Ord`` and ``Eq`` instances of ``Text`` for most of its operations. In the
worst case this means the runtime system has to compare the entire ``Text`` key,
which could be slow when the ``Text`` is large. Another problem is the use of a
list. A list is only an appropriate data structure if it is used like a stack or
if it is used as a store that is eventually traversed and consumed. Once one
finds themselves performing lookups or merges on a list, it is time to use a
different data structure. The last problem is the 3-tuple which we covered
above.

To improve the performance of the ``BindingTable`` we'll use a ``HashMap``. This
should yield better merge performance, and faster writes and reads. However,
this may not fix the root cause of the allocations. So we'll rerun the ticky
report after making the changes to test that we have indeed addressed the
problem.

Attempt 1: Better Data Structures
---------------------------------

We've removed all uses of ``Data.Map`` and replaced them with either a
``HashMap`` or an ``IntMap``. After the changes ``ExpanderState`` now looks
like:

.. code-block:: haskell

  data ExpanderState = ExpanderState
  { _expanderWorld              :: !(World Value)
  , _expanderNextScopeNum       :: !Int
  , _expanderGlobalBindingTable :: !BindingTable
  , _expanderExpansionEnv       :: !ExpansionEnv
  , _expanderTasks              :: [(TaskID, ExpanderLocal, ExpanderTask)]
  , _expanderOriginLocations    :: !(Store SplitCorePtr SrcLoc)
  , _expanderCompletedCore      :: !(Store SplitCorePtr (CoreF TypePatternPtr PatternPtr SplitCorePtr))
  , _expanderCompletedPatterns  :: !(Store PatternPtr (ConstructorPatternF PatternPtr))
  , _expanderCompletedTypePatterns :: !(Store TypePatternPtr TypePattern)
  , _expanderPatternBinders     :: !(Store PatternPtr (Either [PatternPtr] (Scope, Ident, Var, SchemePtr)))
  , _expanderTypePatternBinders :: !(Store TypePatternPtr [(Scope, Ident, Var, SchemePtr)])
  , _expanderCompletedTypes     :: !(Store SplitTypePtr (TyF SplitTypePtr))
  , _expanderCompletedDeclTrees :: !(Store DeclTreePtr (DeclTreeF DeclPtr DeclTreePtr))
  , _expanderCompletedDecls     :: !(Store DeclPtr (Decl SplitTypePtr SchemePtr DeclTreePtr SplitCorePtr))
  , _expanderModuleTop          :: !(Maybe DeclTreePtr)
  , _expanderModuleImports      :: !Imports
  , _expanderModuleExports      :: !Exports
  , _expanderPhaseRoots         :: !(Store Phase Scope)
  , _expanderModuleRoots        :: !(HashMap ModuleName Scope)
  , _expanderKernelBindings     :: !BindingTable
  , _expanderKernelExports      :: !Exports
  , _expanderKernelDatatypes    :: !(HashMap Datatype DatatypeInfo)
  , _expanderKernelConstructors :: !(HashMap Constructor (ConstructorInfo Ty))
  , _expanderKernelValues       :: !(Env Var (SchemePtr, Value))
  , _expanderDeclOutputScopes   :: !(Store DeclOutputScopesPtr ScopeSet)
  , _expanderCurrentEnvs        :: !(Store Phase (Env Var Value))
  , _expanderCurrentTransformerEnvs :: !(Store Phase (Env MacroVar Value))
  , _expanderCurrentDatatypes   :: !(Store Phase (HashMap Datatype DatatypeInfo))
  , _expanderCurrentConstructors :: !(Store Phase (HashMap Constructor (ConstructorInfo Ty)))
  , _expanderCurrentBindingTable :: !BindingTable
  , _expanderExpressionTypes    :: !(Store SplitCorePtr Ty)
  , _expanderCompletedSchemes   :: !(Store SchemePtr (Scheme Ty))
  , _expanderTypeStore          :: !(TypeStore Ty)
  , _expanderKindStore          :: !KindStore
  , _expanderDefTypes           :: !(TypeContext Var SchemePtr) -- ^ Module-level definitions
  }

where a ``Store k v`` is newtype over an ``IntMap`` with some type level
handling for keys:

.. code-block:: haskell

   newtype Store p v = Store { unStore :: IntMap v}
     deriving newtype (Eq, Ord, Show, Semigroup, Monoid, Functor, Foldable)
     deriving stock   Data
   type role Store representational _

now let's check the ticky:

.. code-block:: bash
   07:50:24 ❯ cat ticky | tail -n +20 | sort -k2 -nr | less

   53996388 4319711040          0   3 +.>                  ScopeSet.$wallScopeSets'{v rP2F} (fun)
   60490404 3871385856 2159855520   3 +..                  sat_sQ5D{v} (ScopeSet) (fun) in rP2F
   20257037 1487236040          0   3 iMM                  Binding.$wgo{v r1ric} (fun)
   81547057 1368797696          0   4 SISM                 ScopeSet.$w$sgo4{v rP2E} (fun)
   57730804 1305110352          0   4 SISM                 ScopeSet.$w$sgo1{v rP2D} (fun)
     867831  366262720          0  10 piiSiSL>>>           Parser.Common.$wg{v r3zJ} (fun)
     886661  333384536          0   6 SS>>>>               located{v r1art} (Parser) (fun)
   10521949  330656896          0   3 Lii                  ModuleName.$wgo1{v roEi} (fun)
    4552387  298031744          0   3 ISM                  Expander.$w$sgo4{v r5On7} (fun)
    2699373  259139808          0   4 >SSM                 Syntax.$w$cmapScopes{v rUeh} (fun)
    1351616  237884416     612288   1 T                    f{v s1sRr} (Binding) (fun) in r1rif
    3159635  193376496    1071504   1 M                    go{v s1sS8} (Binding) (fun) in r1rif
    2348710  169685264    1156288   1 M                    go2{v s16Wz} (Env) (fun) in r16zL
    4590545  146897440  183644160   0                      f2{v s1t5Z} (Binding) (thk) in r1ric
     101588  145802400          0   4 MSSM                 $wexpandOneForm{v r5VBM} (Expander) (fun)
    2607448  125157504          0   2 >S                   Syntax.$fHasScopesSyntax_$cmapScopes{v rUeg} (fun)
    1357729  119480152     486976   1 S                    sat_s5YKN{v} (Expander) (fun) in s5YKB
     144974  118076280          0  10 piiiSL>>>>           $wm2{v r1arF} (Parser) (fun)

Notice that the entries to ``unionWith`` and ``isSubsetOf`` have completely
disappeared. In fact, ``isSubsetOf`` is still in the ticky report. It is now
shown as non-allocating:

.. code-block:: bash

   ...
   38279681          0          0   2 MM                   ScopeSet.$sisSubsetOf_$sisSubsetOfX{v rP2u} (fun)
   ...

We've demonstrated progress with the ticky report. Now let's verify that these
changes propagate to the ``implicit-conversion`` test.

.. code-block:: bash

   Test suite klister-tests: RUNNING...
   All tests
     Expander tests
       Core operations
         Shifting core expressions:                       OK
       Mini-tests
         Expected to succeed
           [lambda [x] x]:                                OK
           [lambda [x] [lambda [x] x]]:                   OK
           42:                                            OK
           [lambda [f] [lambda [x] [f x]]]:               OK
           Trivial user macro:                            OK
           Let macro:                                     OK
         Expected to fail
           unbound variable and nothing else:             OK
           unbound variable inside let-syntax:            OK
           refer to a local variable from a future phase: OK
           a macro calls itself:                          OK
       Module tests
         Expected to succeed
           examples/small.kl:                             OK
           examples/two-defs.kl:                          OK
           examples/id-compare.kl:                        OK
           examples/lang.kl:                              OK (0.04s)
           examples/import.kl:                            OK (0.03s)
           examples/phase1.kl:                            OK
           examples/imports-shifted-macro.kl:             OK
           examples/macro-body-shift.kl:                  OK (0.04s)
           examples/test-quasiquote.kl:                   OK (0.04s)
           examples/quasiquote-syntax-test.kl:            OK (0.03s)
           examples/hygiene.kl:                           OK (0.66s)
           examples/defun-test.kl:                        OK (0.03s)
           examples/fun-exports.kl:                       OK
           examples/fun-exports-test.kl:                  OK (0.04s)
           examples/syntax-loc.kl:                        OK
           examples/bound-identifier.kl:                  OK
         Expected to fail
           examples/non-examples/import-phase.kl:         OK
           examples/non-examples/missing-import.kl:       OK
           examples/non-examples/type-errors.kl:          OK
     Hedgehog tests
       runPartialCore . nonPartial = id:                  OK
           ✓ <interactive> passed 10 tests.
       unsplit . split = pure:                            OK
           ✓ <interactive> passed 10 tests.
     Golden tests
       test-quasiquote:                                   OK (0.04s)
       import-scoping:                                    OK
       io:                                                OK (0.03s)
       import-scoping-m1:                                 OK
       tiny-types:                                        OK
       small:                                             OK
       import-scoping-m2:                                 OK
       defun-test:                                        OK (0.03s)
       contract:                                          OK (0.08s)
       int-ops:                                           OK (0.05s)
       implicit-conversion:                               OK (10.42s)
       unknown-type:                                      OK (0.12s)
       let1:                                              OK (0.02s)
       ambiguous-kind:                                    OK
       mcond-test:                                        OK (0.05s)
       error:                                             OK
       prelude-test:                                      OK (0.03s)
       two-defs:                                          OK
       bool:                                              OK (0.04s)
       string:                                            OK
       monad:                                             OK (0.19s)
       pair-datatype:                                     OK (0.08s)
       list-datatype:                                     OK (0.05s)
       temporaries:                                       OK (0.25s)
       hello:                                             OK (0.22s)
       datatype-macro:                                    OK (0.15s)
       exports-macro:                                     OK
       import-renaming:                                   OK (0.06s)
       id-compare:                                        OK
       custom-module:                                     OK (0.30s)
       list-syntax:                                       OK (0.04s)
       let:                                               OK (0.06s)
       lispy-do:                                          OK (0.08s)
       fun-exports-test:                                  OK (0.04s)
       phase1:                                            OK
       import-list-and-do:                                OK (0.15s)
       deep-patterns:                                     OK
       primitive-datatypes:                               OK
       fun-exports:                                       OK
       reader-test:                                       OK (0.49s)
       lang:                                              OK (0.03s)
       either-datatype:                                   OK (0.03s)
       reader:                                            OK (0.24s)
       datatypes:                                         OK
       lets:                                              OK
       macro-body-shift:                                  OK (0.03s)
       bound-identifier:                                  OK
       syntax-loc:                                        OK
       missing-import:                                    OK
       error:                                             OK
       type-errors:                                       OK
       circular-2:                                        OK
       import-phase:                                      OK
       circular-1:                                        OK
       quasiquote-syntax-test:                            OK (0.03s)
       primitives-documentation:                          OK (0.12s)
       hygiene:                                           OK (0.43s)
       one-def:                                           OK
       temporaries-test:                                  OK (0.42s)
       lambda-case-test:                                  OK (0.09s)
       pmatch:                                            OK (0.93s)
       fix:                                               OK
       product-type:                                      OK
       lambda-case:                                       OK (0.07s)
       rpn:                                               OK (1.21s)
       custom-literals:                                   OK (0.31s)
       mcond:                                             OK (0.03s)
       free-identifier-case-test:                         OK (0.13s)
       group:                                             OK
       higher-kinded:                                     OK (0.11s)
       define-syntax-rule:                                OK (0.42s)
       datatype-import:                                   OK (0.04s)
       do:                                                OK (0.10s)
       rpn-test:                                          OK (1.18s)
       imports-shifted-macro:                             OK
       anaphoric-if:                                      OK (0.83s)
       syntax:                                            OK (0.12s)
       defuns:                                            OK (0.82s)
       import-import-renaming:                            OK (0.07s)
       string-syntax:                                     OK (0.04s)
       free-identifier-case:                              OK (0.15s)
       implicit-conversion-test:                          OK (13.55s)
       higher-kinded-patterns:                            OK (0.77s)
       custom-literals-test:                              OK (0.38s)
       double-define:                                     OK (0.28s)
       custom-module-test:                                OK (0.33s)
       which-problem:                                     OK (0.53s)
       incorrect-context:                                 OK (0.03s)
       bound-vs-free:                                     OK (0.25s)
       meta-macro:                                        OK (0.10s)
       integer-syntax:                                    OK (0.04s)
       import:                                            OK (0.03s)
