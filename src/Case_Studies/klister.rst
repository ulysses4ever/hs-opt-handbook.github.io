.. _klister:

..
   Local Variables
.. |klister| replace:: `klister <https://github.com/gelisam/klister/>`__
.. |MegaParsec| replace:: `MegaParsec <https://hackage.haskell.org/package/megaparsec>`__

`Klister First Pass Performance Engineering`
============================================

This chapter is a case study on a first pass of performance engineering for the
|klister| programming language interpreter. This case study should be exemplary
of any system which is shortlived, has distinct phases of input and output, and
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
(``+``) of some type (``.``) and function (``>``). Anytime we observe a function
call to a dictionary ``+`` in a ticky report we can conclude that the function
did not specialize. So from this ticky we know that ``allScopeSets`` has not
specialized and therefore hasn't benefited from possible optimizations if the
function was inlined. The second most allocating code is a SAT'd function
``sat_sOYl``, from its description: ``{v} (ScopeSet) (fun) in rNAX`` we can see
that it is a non-exported name (``{v}``), in the ``(ScopeSet)`` module, it is a
function ``(fun)`` and is a local function in the ``rNAX`` closure, which is the
STG name of the closure for ``allScopeSets`` as shown in description for
``allScopeSets``. So the two most allocating function calls in the interpreter,
when running the testsuite, are due to ``allScopeSets``. Clearly,
``allScopeSets`` is a good target for performance engineering.

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
     ...
       Module tests
         Expected to succeed
         ...
           examples/lang.kl:                              OK (0.04s)
           examples/import.kl:                            OK (0.03s)
           examples/macro-body-shift.kl:                  OK (0.04s)
           examples/test-quasiquote.kl:                   OK (0.04s)
           examples/quasiquote-syntax-test.kl:            OK (0.03s)
           examples/hygiene.kl:                           OK (0.66s)
           examples/defun-test.kl:                        OK (0.03s)
           examples/fun-exports-test.kl:                  OK (0.04s)
     Golden tests
       test-quasiquote:                                   OK (0.04s)
       io:                                                OK (0.03s)
       defun-test:                                        OK (0.03s)
       contract:                                          OK (0.08s)
       int-ops:                                           OK (0.05s)
       implicit-conversion:                               OK (10.42s)
       ...
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

Performance has degraded even though the ticky report showed an improvement! The
``Data.Map`` performance costs must have been eclipsed by some other issue.
However, this is quite surprising. The ``unionWith`` and ``isSubsetOf`` were
very high in the sorted ticky report, so that we do not observe any difference
in wall time *after* fixing the 5th and 6th most allocating function calls is
contrary to what we should expect; even if the total allocations of these
functions are one order of magnitude less than ``allScopeSets``. Let's generate
a heap profile to see what's going on in the heap.

Attempt 2: A Memory Leak Casts a Long Shadow
--------------------------------------------

We'll use eventlog and eventlog2html to observe the heap only on
``implicit-conversion-test``. As a first pass we'll just inspect the types that
are being allocated on the heap by passing ``-hy``:

.. code-block:: bash

   08:46:00 ❯ cabal test --test-show-details=streaming  --test-options='--pattern "implicit-conversion-test" +RTS -hy -l-agu -p -RTS' --ghc-options='-eventlog -rtsopts -O2'

which produces:

.. raw:: html

         <iframe id="scaled-frame" scrolling="no" src="../../../_static/klister/klister-eventlog-implicit-conversion-hy.html"></iframe>

We see that the heap is growing to over 2.8Gb of lists for just one test!
Crucially the shape of this profile is not indicative of a memory leak. A
typical memory leak should look like a pyramid because the program builds up
thunks and then forces them all in relatively short time. What we observe in
this profile is allocations of lists that *never decrease*. Now that we know
what type to look for we can try to correlate this type to a sub-system in the
interpreter. To do so we'll do another heap profile but break down the heap by
module (by using ``-hm`` instead of ``-hy``):

.. code-block:: bash

   08:46:00 ❯ cabal test --test-show-details=streaming  --test-options='--pattern "implicit-conversion-test" +RTS -hm -l-agu -p -RTS' --ghc-options='-eventlog -rtsopts -O2'

.. raw:: html

         <iframe id="scaled-frame" scrolling="no" src="../../../_static/klister/klister-eventlog-implicit-conversion-hm.html"></iframe>

We see that these lists are coming from ``Expander.Monad``. This is suspicious.
We have data being consistently allocated in essentially the state type of a
sub-system. That certainly sounds like a memory leak, but before we can conclude
that it is a memory leak we need to know why this data is retained at all. This
is a good scenario to use :userGuide:`Biological Profiling
<profiling.html#biographical-profiling>` because we want to know: (1) the state
of these objects on the heap and (2) why they are not being collected, that is,
why is GHC's runtime system keeping them alive. For (1) we'll do a biological
profile and for (2) a retainer profile.

Here's the biological profile:

.. code-block:: bash

   08:46:00 ❯ cabal test --test-show-details=streaming  --test-options='--pattern "implicit-conversion-test" +RTS -hb -l-agu -p -RTS' --ghc-options='-eventlog -rtsopts -O2'

.. raw:: html

         <iframe id="scaled-frame" scrolling="no" src="../../../_static/klister/klister-eventlog-implicit-conversion-hb.html"></iframe>

Void! The lists are in a ``void`` state meaning these objects are allocated
``but are never used``. Now we can restate the problem: There is a memory leak
in the ``Expander``, when ``implicit-conversion-test`` is run in the
interpreter, that allocates a total of 121.8 Gb (eventlog shows 116171.68
*MebiBytes* in the detailed tab).

Now to answer why this data is being retained. Here is the retainer profile.

.. code-block:: bash

   09:34:36 ❮ cabal test --enable-profiling --test-show-details=streaming --test-options='--pattern "implicit-conversion-test" +RTS -hr -l-agu -p -RTS' --ghc-options='-eventlog -rtsopts -O2'

   09:47:24 ❯ hp2ps -c klister-tests.hp && ps2pdf klister-tests.ps

.. note::

   Eventlog threw an exception for this retainer profile. So I've resorted to
   use the classic tools: ``hp2ps`` and ``ps2pdf`` to render the profile.

.. image:: /_static/klister/klister-eventlog-implicit-conversion-hr.png
   :width: 800

The retainer profile clearly shows that ``currentEnv`` is keeping this data
alive and now has the distinguishing profile of a memory leak. Let's look at
that function:

.. code-block:: haskell

   -- in Expander.Monad

   currentEnv :: Expand VEnv
   currentEnv = do
     phase <- currentPhase
     globalEnv <- fromMaybe mempty . view (expanderWorld . worldEnvironments . at phase) <$> getState
     localEnv  <- fromMaybe mempty . view (expanderCurrentEnvs . at phase) <$> getState
     return $ globalEnv <> localEnv

This code is reading from the ``Expander`` state twice to retrieve ``globalEnv``
and ``localEnv`` and the returning the union of these two environments. Because
this code is monadic, there is a high probability that these projections from
the state to retrieve ``globalEnv`` and ``localEnv`` are lazy. Furthermore, the
``return`` statement is clearly lazy. In general, unless the result of a monadic
action *needs* to be consumed lazily there is little reason to not make
it strict in the return. Using a lazy return risks thunks accumulating from
bind. In this case, if the result is not immediately demanded, then this code
will allocate a thunk for ``phase``, ``globalEnv``, ``localEnv`` and the merge
of both ``globalEnv`` and ``localEnv``.

Before changing the code, let's first inspect the types. Here is the type for
``Expand``:

.. code-block:: haskell

   newtype Expand a = Expand
     { runExpand :: ReaderT ExpanderContext (ExceptT ExpansionErr IO) a
     }
     deriving (Functor, Applicative, Monad, MonadError ExpansionErr, MonadIO, MonadReader ExpanderContext)

   data ExpanderContext = ExpanderContext
     { _expanderLocal :: !ExpanderLocal
     , _expanderState :: IORef ExpanderState
     }

Where ``ExpanderState`` was shown above. So we have a classic `ReaderT over IO
<https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/>`_ pattern.
Meaning that the laziness of any state updates depend on the strictness of
functions operating on ``ExpanderContext``. Next let's check the types of
``globalEnv`` and ``localEnv``:

.. code-block:: haskell

   -- in Expander.Monad.hs
   type VEnv = Env Var Value

   -- in Env.hs
   newtype Env v a = Env (IntMap (Ident, a))
     deriving newtype (Eq, Monoid, Semigroup, Show)
     deriving stock Functor

   -- in World.hs

   data World a = World
     { _worldEnvironments :: !(Store Phase (Env Var a))
     , _worldTypeContexts :: !(TypeContext Var SchemePtr)
     , _worldTransformerEnvironments :: !(Store Phase (Env MacroVar a))
     , _worldModules      :: !(HashMap ModuleName CompleteModule)
     , _worldVisited      :: !(HashMap ModuleName (Set Phase))
     , _worldExports      :: !(HashMap ModuleName Exports)
     , _worldEvaluated    :: !(HashMap ModuleName [EvalResult])
     , _worldDatatypes    :: !(Store Phase (HashMap Datatype DatatypeInfo))
     , _worldConstructors :: !(Store Phase (HashMap Constructor (ConstructorInfo Ty)))
     , _worldLocation     :: FilePath
     }

``currentEnv``, the leaky function, returns a ``Expand VEnv``, ``VEnv`` is a
``Env Var Value`` where an ``Env`` is basically an ``IntMap``. Thus
``globalEnv`` and ``localEnv`` are both ``IntMap`` that stores a tuple of
``(Ident, Value)``. Here is the type of ``Value``:

.. code-block:: haskell

   -- in Value.hs
   data Value
     = ValueClosure Closure
     | ValueSyntax Syntax
     | ValueMacroAction MacroAction
     | ValueIOAction (IO Value)
     | ValueOutputPort Handle
     | ValueInteger Integer
     | ValueCtor Constructor [Value]
     | ValueType Ty
     | ValueString Text

Notice that ``ValueCtor`` holds a lazy list of ``Value``. Should
``implicit-tests`` create many ``ValueCtor`` then the expander state will blow
up. Let's test this and make ``Value`` strict and then generate another
biographical profile to observe the change:

.. code-block:: haskell

   -- in Value.hs
   data Value
     = ValueClosure !Closure
     | ValueSyntax  !Syntax
     | ValueMacroAction !MacroAction
     | ValueIOAction   !(IO Value)
     | ValueOutputPort !Handle
     | ValueInteger    !Integer
     | ValueCtor    !Constructor ![Value]
     | ValueType    !Ty
     | ValueString  !Text

.. raw:: html

         <iframe id="scaled-frame" scrolling="no"
         src="../../../_static/klister/klister-eventlog-implicit-conversion-strict-value.html"></iframe>

Unfortunately, the change made no difference. We'll revert the change and try
making the monadic action strict in its return:

.. code-block:: haskell

   -- in Expander.Monad

   currentEnv :: Expand VEnv
   currentEnv = do
     phase <- currentPhase
     globalEnv <- fromMaybe mempty . view (expanderWorld . worldEnvironments . at phase) <$> getState
     localEnv  <- fromMaybe mempty . view (expanderCurrentEnvs . at phase) <$> getState
     return $! globalEnv <> localEnv  -- new

which results in this profile:


.. code-block:: bash

   08:46:00 ❯ cabal test --test-show-details=streaming  --test-options='--pattern "implicit-conversion-test" +RTS -hb -l-agu -p -RTS' --ghc-options='-eventlog -rtsopts -O2'

.. raw:: html

         <iframe id="scaled-frame" scrolling="no" src="../../../_static/klister/klister-eventlog-implicit-conversion-currentEnv-fixed.html"></iframe>

A significant improvement! Instead of 121.8 Gb the profile shows total
allocation in ``void`` of 4.62 Gb (4404.22 MiB) which is a 30x reduction.


Attempt 3: Still Too Much Void
------------------------------

However, there is still a lot of ``void`` in the heap profile. This is a good
scenario for info-table profiling. Info-table profiling relates source code to
closures so with it we can see where in the source code the ``void`` is
originating.

.. code-block:: bash

   11:16:31 ❮ cabal test --test-show-details=streaming --test-options='--pattern "implicit-conversion-test" +RTS -hi -i0.05 -l -RTS' --ghc-options='-eventlog -rtsopts -O2 -finfo-table-map -fdistinct-construct
   or-tables'

and the profile is rendered in eventlog:

.. raw:: html

         <iframe id="scaled-frame" scrolling="no" src="../../../_static/klister/klister-eventlog-implicit-conversion-ipe-allscopeset.html"></iframe>

Notice that the legend displays the :term:`Info Table Address` instead of the
closure type, module, or biography. From the profile we find that ``0x7c41d0``
and ``0xc0c330`` are responsible for the ``void`` allocation. The detailed tab
maps these addresses directly to source code. In the detailed tab, we see that
``0x7c41d0`` has the description ``sat_sN17_info``, the closure type ``THUNK``,
the type ``f a``, and is in the module ``ScopeSet`` at line 146. Thus we now
know that the ``void`` is originating from thunks that are never resolved. That
line is exactly the local ``combine`` function in ``allScopeSets``. Recall that
we also observed ``allScopeSets`` doing the most allocation in the ticky profile
above. Here is the source code:

.. code-block:: haskell

   allScopeSets :: Data d => Traversal' d ScopeSet
   allScopeSets = allScopeSets'
     where
       allScopeSets' :: forall f d. (Applicative f, Data d)
                     => (ScopeSet -> f ScopeSet)
                     -> d -> f d
       allScopeSets' f = gmapA go
         where
           go :: forall a. Data a => a -> f a
           go a = case eqT @a @ScopeSet of
             Just Refl -> f a
             Nothing -> allScopeSets' f a

       -- A variant of Data.Data.gmapM which uses Applicative instead of Monad
       gmapA :: forall f d. (Applicative f, Data d)
             => (forall x. Data x => x -> f x)
             -> d -> f d
       gmapA g = gfoldl combine pure
         where
           combine :: Data a => f (a -> b) -> a -> f b
           combine ff a = (<*>) ff (g a)

This code is exceedingly polymorphic and is effectively asking GHC to generate
traversals over many different data types. We can observe exactly which data
types by checking Core. Note that I am only showing a very small snippet of the
Core that is generated for just ``gmapA`` because that is the driver of
``combine``:

.. code-block:: haskell

   ScopeSet.$w$cgmapMp [InlPrag=[2]]
     :: forall {m :: * -> *}.
        (forall a b. m a -> (a -> m b) -> m b)
        -> (forall a. a -> m a)
        -> (forall a. m a)
        -> (forall a. m a -> m a -> m a)
        -> (forall d. Data d => d -> m d)
        -> ScopeSet
        -> m ScopeSet
   [GblId,
    Arity=6,
    Str=<SCS(C1(L))><L><L><LCL(C1(L))><LCL(C1(L))><MP(L,L)>,
    Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
            WorkFree=True, Expandable=True,
            Guidance=IF_ARGS [300 360 0 120 120 20] 640 0}]
   ScopeSet.$w$cgmapMp
     = \ (@(m_s1ixv :: * -> *))
         (ww_s1ixG
            :: forall a b. m_s1ixv a -> (a -> m_s1ixv b) -> m_s1ixv b)
         (ww1_s1ixI :: forall a. a -> m_s1ixv a)
         (ww2_s1ixK :: forall a. m_s1ixv a)
         (ww3_s1ixL :: forall a. m_s1ixv a -> m_s1ixv a -> m_s1ixv a)
         (w_s1ixx :: forall d. Data d => d -> m_s1ixv d)
         (w1_s1ixy :: ScopeSet) ->
         ww_s1ixG
           @(ScopeSet, Bool)
           @ScopeSet
           (src<src/ScopeSet.hs:52:13-16>
        ...
         ww_s1ixG
           @(Store Phase (Set Scope) -> ScopeSet, Bool)
           @(ScopeSet, Bool)
           (let {
              lvl15_s1iqy :: m_s1ixv (Set Scope)
              [LclId]
              lvl15_s1iqy
                = w_s1ixx @(Set Scope) ScopeSet.$fDataScopeSet3 a1_a1hFH } in
            ww_s1ixG
              @(Set Scope -> Store Phase (Set Scope) -> ScopeSet, Bool)
              @(Store Phase (Set Scope) -> ScopeSet, Bool)
              (ww1_s1ixI
                 @(Set Scope -> Store Phase (Set Scope) -> ScopeSet, Bool)
                 ScopeSet.$fDataScopeSet1)

First notice that the Core name is ``ScopeSet.$w$cgmapMp`` not
``ScopeSet$w$cgmapA``. This code snippet is from *only one* of the ``gmapA``
functions that are generated, and is ~100 lines of Core; in total there are six
versions that are generated:

.. code-block:: haskell


   ScopeSet.$fDataScopeSet [InlPrag=CONLIKE] :: Data ScopeSet
   ...
   ScopeSet.$fDataScopeSet_$cgmapQr
   ScopeSet.$fDataScopeSet_$cgmapQ
   ScopeSet.$fDataScopeSet_$cgmapQi
   ScopeSet.$fDataScopeSet_$cgmapM
   ScopeSet.$fDataScopeSet_$cgmapMp
   ScopeSet.$fDataScopeSet_$cgmapMo

More importantly the types that this ``gmapA`` are traversing are displayed via
coercions prefixed by ``@``. For example, ``@(Set Scope -> Store Phase (Set
Scope) -> ScopeSet, Bool)`` and ``@(Store Phase (Set Scope) -> ScopeSet, Bool)``.
