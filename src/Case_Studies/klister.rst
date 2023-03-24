.. _klister:

..
   Local Variables
.. |klister| replace:: `klister <https://github.com/gelisam/klister/>`__

`Klister and the Ever Growing Cache`
====================================

This chapter is a case study on the first pass of performance engineering for
the |klister| programming language. This case study should be exemplary of any
system which is shortlived, has a distinct phase of input and output, and
maintains state. To diagnose the performance issues this case study uses
:ref:`Heap Profiling<Heap Profiling Chapter` with :ref:`Eventlog <EventLog
Chapter>`, :ref:`Info Table Profiling <IPE Chapter>`, and ... TODO

Introduction to the System
--------------------------

Klister is an interpreted programming language. The exact kind of language, and
its features, are not relevant for our purposes. Instead, all we need to know
about the system is that it inputs a program, parses the program, maintains
mutable stores of variables and outputs a result.

Characterizing the Problem
--------------------------

We identify two problems. First, we find suspicious uses of ``Data.Map`` by a
quick read through the interpreter code:

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
``IntMap`` has several advantages over ``Data.Map``. ``Data.Map`` is an
implementation *size balanced binary trees* (see the `hackage
<https://hackage.haskell.org/package/containers-0.6.7/docs/Data-Map.html>`_
docs) thus it periodically needs to be rebalanced. This balancing slows down
writes (that is, adds work to ``Data.Map.insert``) and consequently merges,
because any merge of any two trees may invoke a rebalancing to maintain the
balanced invariant of the tree. In contrast, ``IntMap`` is a big-endian
PATRICIA Trie which never require balancing. Compared to ``Data.Map``, an
``IntMap`` provides faster writes at the cost of slightly slower reads.
Additionally, ``IntMap`` uses less totaly memory than ``Data.Map``. See Chris
Done's comparisons `here <https://github.com/haskell-perf/dictionaries>`_.

Besides ``Map``'s indexed over ``Int`` and ``Integer`` like types, klister also
uses many ``Map``'s indexed over ``String`` like types. For example,
``_expanderKernelDatatypes`` is a ``Map Datatype DatatypeInfo``, where
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

Indexing over ``Data.Map`` over ``String`` like types is generally a performance
anti-pattern because the ``Ord`` and ``Eq`` instance on ``String`` will need to
check the entire ``String`` in the worst case. This means that all operations
which compare keys can slow down. For klister, this is not a problem yet because
programs are still small and therefore it is likely that these maps are not
a dominating factor yet.

A better datastructure for maps indexed over ``String`` like types is
``Data.HashMap`` from the ``unordered-containers`` library. These maps are
*Hashed Array Mapped Trie's*, so they index over a unique ``hash`` which
represents the key type. Thus, these data structures are very efficient for any
key type where an equality check could be expensive; such as ``String``,
``Text`` or other algebraic data types.

The second problem is less speculative; the test called
``implicit-conversion-test`` seems to be slow. Klister does not have a benchmark
suite, but does have a testsuite written in :ref:`tasty <Tasty Chapter>` which
outputs the wall time of each test. So we can compare this test to every other
test that reports a time:

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

Notice that both ``implicit-conversion`` and ``implicit-conversion-test`` pass
in 7 and 9 *seconds*, whereas each other test passes in well under a second
(except ``higher-kinded-patterns``). Clearly something is amiss.


Restate the Problem
-------------------

We have identified two problems: (1) Suspiciuos ``Data.Map`` and (2) a slow
test. If the maps are a problematic factor then we should expect a lot of
allocations to come from ``Data.Map.insert``, ``(==)``, ``Ord`` instances and
the functions ``Data.Map.Internal.balanceR`` and ``Data.Map.Internal.balanceL``.
This is a good opportunity to :ref:`not think and look <Don't think look>` with
a :ref:`tickyticky <Ticky Chapter>` chapter.

So let's run a ticky report:

.. code-block:: bash

   14:32:29 ❯ cabal test --test-show-details=streaming --test-options='+RTS -rticky -RTS' --ghc-options='-rtsopts -ticky'
     Build profile: -w ghc-9.2.4 -O1
     ...

and check the results sorted by allocations:

.. code-block:: bash

   ~/programming/klister main*
   14:45:37 ❯ cat ticky | tail -n +20 | sort -k2 -nr | less

   53739709 4299176720          0   3 +.>                  ScopeSet.$wallScopeSets'{v rIaM} (fun)
   60292448 3858716672          0   3 +..                  sat_sJxV{v} (ScopeSet) (fun) in rIaM
   81547057 1368797696          0   4 SISM                 ScopeSet.$w$sgo4{v rIaL} (fun)
   57730804 1305110352          0   4 SISM                 ScopeSet.$w$sgo1{v rIaJ} (fun)
   61143424  841913088          0   2 SM                   ScopeSet.isSubsetOf_go15{v rIaR} (fun)
   17961626  421056776          0   3 >MM                  Binding.$fMonoidBindingTable_$sunionWith{v r1g8l} (fun)
   ...
   203666   24439920            0   2 SS                   ScopeSet.insertAtPhase{v rI8V} (fun)
   275489   23497624            0   1 M                    f1{v sJG1} (ScopeSet) (fun) in rI8R
   1684110  20563200            0   3 SSM                  Expander.$sinsert_$sgo4{v r4Ege} (fun)
   1746731  14568192            0   4 SS.M                 Evaluator.$sinsert_$sgo15{v r35NF} (fun)

We see that insertions are not the top of the list but still allocate a fair
amount of memory (around 195 MegaBits). This is not entirely unexpected as we
know the programs we're running are smaller than one that will be run in the
future. However, we do see that the 5th and 6th most allocating functions called
when the testsuite is run are ``ScopeSet.isSubsetOf`` and
``Binding.$fMonoidBindingTable_$unionWith``. Let's check these functions in the
source code:

.. code-block:: haskell






Don't Think Look
----------------
