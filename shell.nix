with (import <nixpkgs> { });

haskell.lib.buildStackProject {
  name = "myEnv";
  buildInputs = [ glpk pcre zlib ];
}
