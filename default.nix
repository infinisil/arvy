{ pkgs ? import (fetchTarball {
    url = "https://github.com/infinisil/nixpkgs/tarball/c382915e2817c694a1eee4fc60d9562c972b9f15";
    sha256 = "1p3j1ddzd9pjlvgl51xjj38nykhiw64y9scg02i1z1i81nvqklsi";
  }) {}
}:
let

  inherit (pkgs) lib;
  hlib = pkgs.haskell.lib;

  src = lib.sourceByRegex ./. [
    "lib.*"
    "LICENSE"
    "Setup.hs"
    "arvy.cabal"
  ];

  hpkgs = pkgs.haskell.packages.ghcHEAD.override (old: {
    overrides = lib.composeExtensions (old.overrides or (self: super: {})) (self: super: {



      arvy = super.callCabal2nix "arvy" src {};

      QuickCheck = hlib.appendPatch (hlib.doJailbreak super.QuickCheck) (pkgs.fetchpatch {
        name = "QuickCheck-MonadFail.patch";
        url = "https://patch-diff.githubusercontent.com/raw/nick8325/quickcheck/pull/255.patch";
        sha256 = "0v0i1lkszcami3rf3yba59cmi20nq5f6d366yg2hnjg8h2m4haxs";
      });

      splitmix = hlib.doJailbreak super.splitmix;

      # Break infinite recursion
      base-compat-batteries = hlib.dontCheck super.base-compat-batteries;

      #th-abstraction = hlib.doJailbreak super.th-abstraction;
      th-abstraction = hlib.doJailbreak self.th-abstraction_0_3_1_0;

      ghc-tcplugins-extra = (hlib.doJailbreak super.ghc-tcplugins-extra).overrideAttrs (old: {
        patches = old.patches or [] ++ [ (pkgs.writeText "th.patch" ''
          diff --git a/src/GHC/TcPluginM/Extra.hs b/src/GHC/TcPluginM/Extra.hs
          index 2f95012..a347a7b 100644
          --- a/src/GHC/TcPluginM/Extra.hs
          +++ b/src/GHC/TcPluginM/Extra.hs
          @@ -371,8 +371,8 @@ substType _subst t@(ForAllTy _tv _ty) =
             -- TODO: Is it safe to do "dumb" substitution under binders?
             -- ForAllTy tv (substType subst ty)
             t
          -substType subst (FunTy t1 t2) =
          -  FunTy (substType subst t1) (substType subst t2)
          +substType subst (FunTy f t1 t2) =
          +  FunTy f (substType subst t1) (substType subst t2)
           substType _ l@(LitTy _) = l
           substType subst (CastTy ty co) =
             CastTy (substType subst ty) co
        '') ];
      });

      primitive = hlib.dontCheck (hlib.doJailbreak super.primitive_0_7_0_0);


      inspection-testing = hlib.appendPatch (hlib.doJailbreak super.inspection-testing) (pkgs.writeText "ghc-lib.patch" ''
        diff --git a/src/Test/Inspection/Core.hs b/src/Test/Inspection/Core.hs
        index b70bb61..8ac2b10 100644
        --- a/src/Test/Inspection/Core.hs
        +++ b/src/Test/Inspection/Core.hs
        @@ -226,7 +226,7 @@ allTyCons predicate slice = listToMaybe [ (v,e) | (v,e) <- slice, not (go e) ]
                                 -- ↑ This is the crucial bit
             goT (ForAllTy _ t)   = goT t
         #if MIN_VERSION_GLASGOW_HASKELL(8,2,0,0)
        -    goT (FunTy t1 t2)    = goT t1 && goT t2
        +    goT (FunTy _ t1 t2)    = goT t1 && goT t2
         #endif
             goT (LitTy _)        = True
             goT (CastTy t _)     = goT t
        diff --git a/src/Test/Inspection/Plugin.hs b/src/Test/Inspection/Plugin.hs
        index 8dbdea1..dbdcddb 100644
        --- a/src/Test/Inspection/Plugin.hs
        +++ b/src/Test/Inspection/Plugin.hs
        @@ -265,7 +265,7 @@ proofPass :: UponFailure -> ReportingMode -> ModGuts -> CoreM ModGuts
         proofPass upon_failure report guts = do
             dflags <- getDynFlags
             when (optLevel dflags < 1) $
        -        warnMsg $ fsep $ map text $ words "Test.Inspection: Compilation without -O detected. Expect optimizations to fail."
        +        warnMsg NoReason $ fsep $ map text $ words "Test.Inspection: Compilation without -O detected. Expect optimizations to fail."
         
             let (guts', obligations) = extractObligations guts
             (toStore, stats) <- (concat `bimap` M.unionsWith (+)) . unzip <$>
        @@ -291,7 +291,7 @@ proofPass upon_failure report guts = do
                         errorMsg $ text "inspection testing unsuccessful" $$ summary_message
                         liftIO $ exitFailure -- kill the compiler. Is there a nicer way?
                     KeepGoing -> do
        -                warnMsg $ text "inspection testing unsuccessful" $$ summary_message
        +                warnMsg NoReason $ text "inspection testing unsuccessful" $$ summary_message
         
             return guts'''
      '');

      algebraic-graphs = hlib.dontCheck super.algebraic-graphs;

      polysemy = hlib.doJailbreak (hlib.dontCheck (super.polysemy.overrideAttrs (old: {
        patches = old.patches or [] ++ [ (pkgs.writeText "th.patch" ''
          diff --git a/src/Polysemy/Internal/TH/Effect.hs b/src/Polysemy/Internal/TH/Effect.hs
          index 2192ce9..7e44c75 100644
          --- a/src/Polysemy/Internal/TH/Effect.hs
          +++ b/src/Polysemy/Internal/TH/Effect.hs
          @@ -133,8 +133,10 @@ genFreer should_mk_sigs type_name = do
             dt_info    <- reifyDatatype type_name
             cl_infos   <- traverse (mkCLInfo dt_info) $ datatypeCons dt_info
             tyfams_on  <- isExtEnabled TypeFamilies
          -  def_mod_fi <- sequence [ TySynInstD '''DefiningModule
          -                             . TySynEqn [ConT $ datatypeName dt_info]
          +  def_mod_fi <- sequence [ TySynInstD
          +                             . TySynEqn Nothing (AppT
          +                                (ConT '''DefiningModule)
          +                                (ConT $ datatypeName dt_info))
                                        . LitT
                                        . StrTyLit
                                        . loc_module
        '') ];
      })));

      polysemy-zoo = hlib.addBuildDepend (hlib.dontCheck super.polysemy-zoo) self.polysemy-plugin;

      tf-random = hlib.dontCheck super.tf-random;

      constraints = hlib.dontCheck self.constraints_0_11;

      hashable = hlib.doJailbreak (hlib.dontCheck super.hashable);

      polysemy-plugin = hlib.dontCheck (hlib.doJailbreak super.polysemy-plugin);

      generic-deriving = hlib.dontCheck super.generic-deriving;

      cereal = hlib.dontCheck super.cereal;

      scientific = hlib.dontCheck super.scientific;

      base-orphans = hlib.dontCheck super.base-orphans;

      #cabal-doctest = hlib.doJailbreak super.cabal-doctest;

      flexible-defaults = super.flexible-defaults.overrideAttrs (old: {
        postPatch = old.postPatch or "" + ''
          sed -i src/Language/Haskell/TH/FlexibleDefaults/DSL.hs \
            -e 's/fail/error/'
        '';
      });

      bytes = hlib.addBuildDepend (hlib.overrideCabal ((hlib.doJailbreak (hlib.dontCheck super.bytes)).overrideAttrs (old: {
        src = pkgs.fetchFromGitHub {
          owner = "ekmett";
          repo = "bytes";
          rev = "3bc0baa73cf215d70e95304cd55ff4b7991d7b57";
          sha256 = "0m8r1nx3y36i1bb0mndfxjdhdmybx8shyj7b6s5043w0n4ic3xci";
        };
      })) (old: {
        editedCabalFile = null;
      })) self.binary-orphans_1_0_1;

      binary-orphans_1_0_1 = hlib.dontCheck super.binary-orphans_1_0_1;

      test-framework = hlib.doJailbreak super.test-framework;

      async = hlib.doJailbreak super.async;

      random-fu = super.random-fu.overrideAttrs (old: {
        src = pkgs.fetchFromGitHub {
          owner = "mokus0";
          repo = "random-fu";
          rev = "f4139d1ca0f3d10ebc704ef81e9a8146826caa97";
          sha256 = "0cizrfngi31li6jqpvfq2w0rkdaqxhskgg4lixjcq45rbday8wwg";
        };
        sourceRoot = "source/random-fu";
        patches = old.patches or [] ++ [(pkgs.writeText "fail.patch" ''
          diff --git a/src/Data/Random/Distribution/Categorical.hs b/src/Data/Random/Distribution/Categorical.hs
          index ac5dc33..76f3a8d 100644
          --- a/src/Data/Random/Distribution/Categorical.hs
          +++ b/src/Data/Random/Distribution/Categorical.hs
          @@ -112,7 +112,7 @@ instance (Num p, Read p, Read a) => Read (Categorical p a) where
           
           instance (Fractional p, Ord p, Distribution Uniform p) => Distribution (Categorical p) a where
               rvarT (Categorical ds)
          -        | V.null ds = fail "categorical distribution over empty set cannot be sampled"
          +        | V.null ds = error "categorical distribution over empty set cannot be sampled"
                   | n == 1    = return (snd (V.head ds))
                   | otherwise = do
                       u <- uniformT 0 (fst (V.last ds))
          @@ -156,10 +156,6 @@ instance Traversable (Categorical p) where
           instance Fractional p => Monad (Categorical p) where
               return x = Categorical (V.singleton (1, x))
               
          -    -- I'm not entirely sure whether this is a valid form of failure; see next
          -    -- set of comments.
          -    fail _ = Categorical V.empty
          -    
               -- Should the normalize step be included here, or should normalization
               -- be assumed?  It seems like there is (at least) 1 valid situation where
               -- non-normal results would arise:  the distribution being modeled is 
          diff --git a/random-fu/src/Data/Random/Distribution/ChiSquare.hs b/random-fu/src/Data/Random/Distribution/ChiSquare.hs
          index 41ac495..ea05c43 100644
          --- a/src/Data/Random/Distribution/ChiSquare.hs
          +++ b/src/Data/Random/Distribution/ChiSquare.hs
          @@ -22,7 +22,7 @@ instance (Fractional t, Distribution Gamma t) => Distribution ChiSquare t where
               rvarT (ChiSquare 0) = return 0
               rvarT (ChiSquare n)
                   | n > 0     = gammaT (0.5 * fromInteger n) 2
          -        | otherwise = fail "chi-square distribution: degrees of freedom must be positive"
          +        | otherwise = error "chi-square distribution: degrees of freedom must be positive"
           
           instance (Real t, Distribution ChiSquare t) => CDF ChiSquare t where
               cdf (ChiSquare n) x = incompleteGamma (0.5 * fromInteger n) (0.5 * realToFrac x)
          diff --git a/random-fu/src/Data/Random/Distribution/T.hs b/random-fu/src/Data/Random/Distribution/T.hs
          index af8b6e9..ddd7b43 100644
          --- a/src/Data/Random/Distribution/T.hs
          +++ b/src/Data/Random/Distribution/T.hs
          @@ -34,7 +34,7 @@ instance (Floating a, Distribution Normal a, Distribution ChiSquare a) => Distri
                       x <- stdNormalT
                       y <- chiSquareT n
                       return (x * sqrt (fromInteger n / y))
          -        | otherwise = fail "Student's t-distribution: degrees of freedom must be positive"
          +        | otherwise = error "Student's t-distribution: degrees of freedom must be positive"
           
           instance (Real a, Distribution T a) => CDF T a where
               cdf (T n) t = incompleteBeta v2 v2 x
        '')];
      });



      comonad = hlib.dontCheck super.comonad;
      distributive = hlib.dontCheck super.distributive;
      log-domain = hlib.dontCheck super.log-domain;
      semigroupoids = hlib.dontCheck super.semigroupoids;

      regex-base = super.regex-base.overrideAttrs (old: {
        postPatch = old.postPatch or "" + ''
          sed -i Text/Regex/Base/* \
            -e 's/Monad m/MonadFail m/'
        '';
      });

      regex-posix = super.regex-posix.overrideAttrs (old: {
        postPatch = old.postPatch or "" + ''
          sed -i Text/Regex/Posix/Wrap.hsc \
            -e 's/Monad m/MonadFail m/'
        '';
      });

      cabal-doctest = null;

      integer-logarithms = hlib.doJailbreak super.integer-logarithms;

      dlist = (hlib.doJailbreak super.dlist).overrideAttrs (old: {
        src = pkgs.fetchFromGitHub {
          owner = "spl";
          repo = "dlist";
          rev = "fccd135132d892cb5f22d1717e5e59b287930644";
          sha256 = "1i8xiv84zz5ckfwwam5f3amwqppx4g56s6sa9fam5rx4dyzqnahs";
        };
      });

      doctest = hlib.doJailbreak super.doctest;

      bifunctors = hlib.dontCheck super.bifunctors;

      optparse-applicative = (hlib.doJailbreak (hlib.dontCheck super.optparse-applicative)).overrideAttrs (old: {

        src = pkgs.fetchFromGitHub {
          owner = "pcapriotti";
          repo = "optparse-applicative";
          rev = "ad2fa7f6430801cf2f537e82da105e298dc365fd";
          sha256 = "02v8qs06nsyvsg0j3m1aw6h0pw7dsxghi5f003qqzdzdkyxcryyv";
        };

      });

      rvar = super.rvar.overrideAttrs (old: {
        src = pkgs.fetchFromGitHub {
          owner = "mokus0";
          repo = "random-fu";
          rev = "f4139d1ca0f3d10ebc704ef81e9a8146826caa97";
          sha256 = "0cizrfngi31li6jqpvfq2w0rkdaqxhskgg4lixjcq45rbday8wwg";
        };
        sourceRoot = "source/rvar";
        patches = old.patches or [] ++ [(pkgs.writeText "fail.patch" ''
          diff --git a/src/Data/RVar.hs b/src/Data/RVar.hs
          index e6854aa..7eee30e 100644
          --- a/src/Data/RVar.hs
          +++ b/src/Data/RVar.hs
          @@ -197,7 +197,6 @@ instance Functor (RVarT n) where
           
           instance Monad (RVarT n) where
               return x = RVarT (return $! x)
          -    fail s   = RVarT (fail s)
               (RVarT m) >>= k = RVarT (m >>= \x -> x `seq` unRVarT (k x))
           
           instance MonadRandom (RVarT n) where
        '')];
      });

      vector-th-unbox = hlib.appendPatch super.vector-th-unbox (pkgs.writeText "th.patch" ''
        diff --git a/Data/Vector/Unboxed/Deriving.hs b/Data/Vector/Unboxed/Deriving.hs
        index 52380b6..dc25d07 100644
        --- a/Data/Vector/Unboxed/Deriving.hs
        +++ b/Data/Vector/Unboxed/Deriving.hs
        @@ -126,7 +126,7 @@ derivingUnbox name argsQ toRepQ fromRepQ = do
         # define MAYBE_KIND
         # define MAYBE_OVERLAP
         #endif
        -    let newtypeMVector = NewtypeInstD [] '''MVector [s, typ] MAYBE_KIND
        +    let newtypeMVector = NewtypeInstD [] Nothing (AppT (AppT (ConT '''MVector) s) typ) MAYBE_KIND
                     (NormalC mvName [(lazy, ConT '''MVector `AppT` s `AppT` rep)]) []
             let mvCon = ConE mvName
             let instanceMVector = InstanceD MAYBE_OVERLAP cxts
        @@ -147,7 +147,7 @@ derivingUnbox name argsQ toRepQ fromRepQ = do
                     , wrap 'M.basicUnsafeMove       [mv, mv']   id
                     , wrap 'M.basicUnsafeGrow       [mv, n]     (liftE mvCon) ]
         
        -    let newtypeVector = NewtypeInstD [] '''Vector [typ] MAYBE_KIND
        +    let newtypeVector = NewtypeInstD [] Nothing (AppT (ConT '''Vector) typ) MAYBE_KIND
                     (NormalC vName [(lazy, ConT '''Vector `AppT` rep)]) []
             let vCon  = ConE vName
             let instanceVector = InstanceD MAYBE_OVERLAP cxts
      '');

      vector = hlib.doJailbreak super.vector;


    });
  });

  pkg = hpkgs.arvy;

  env = pkg.env.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs or [] ++ [ pkgs.haskellPackages.cabal-install ];
  });

in pkg // { inherit env; }
