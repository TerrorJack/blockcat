{-# OPTIONS_GHC -threaded -with-rtsopts="-N -I0 -qg -qb" #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable
import Data.Maybe
import Development.Shake
import Development.Shake.FilePath
import Distribution.Simple
import Distribution.Simple.Setup
import System.Environment
import System.Info

buildBinaryen :: FilePath -> IO ()
buildBinaryen libdir = do
    cxx <- fromMaybe "g++" <$> lookupEnv "CXX"
    let (libTarget, dynlibTarget) =
            if isWindows
                then ( libdir </> "libbinaryen.dll.a"
                     , libdir </> "libbinaryen.dll")
                else (libdir </> "libbinaryen.a", libdir </> "libbinaryen.so")
    shake shakeOptions {shakeThreads = 0, shakeProgress = progressSimple} $ do
        want [libTarget, dynlibTarget]
        libTarget %> \_ -> do
            need objs
            command_ [] "ar" ("crs" : libTarget : objs)
        dynlibTarget %> \_ -> do
            need objs
            command_ [] cxx ("-shared" : "-o" : dynlibTarget : objs)
        for_ srcs $ \(src, headers) ->
            (srcPrefix </> src <.> "obj") %> \obj -> do
                need [srcPrefix </> h | h <- headers]
                command_
                    []
                    cxx
                    [ "-I"
                    , srcPrefix
                    , "-std=c++11"
                    , "-msse2"
                    , "-mfpmath=sse"
                    , "-Wall"
                    , "-Werror"
                    , "-Wextra"
                    , "-Wno-unused-parameter"
                    , "-fno-omit-frame-pointer"
                    , "-fPIC"
                    , "-O2"
                    , "-UNDEBUG"
                    , "-o"
                    , obj
                    , "-c"
                    , srcPrefix </> src
                    ]
        cbitsPrefix </> "utils.cpp.obj" %> \obj -> do
            need [cbitsPrefix </> "utils.h", srcPrefix </> "binaryen-c.h"]
            command_
                []
                cxx
                [ "-I"
                , srcPrefix
                , "-I"
                , cbitsPrefix
                , "-std=c++11"
                , "-Wall"
                , "-Werror"
                , "-Wextra"
                , "-fno-omit-frame-pointer"
                , "-O2"
                , "-UNDEBUG"
                , "-o"
                , obj
                , "-c"
                , cbitsPrefix </> "utils.cpp"
                ]
  where
    binaryenPrefix = "binaryen"
    srcPrefix = binaryenPrefix </> "src"
    cbitsPrefix = "cbits"
    isWindows = os == "mingw32"
    objs =
        (cbitsPrefix </> "utils.cpp.obj") :
        [srcPrefix </> src <.> "obj" | (src, _) <- srcs]
    srcs =
        [ ( "binaryen-c.cpp"
          , [ "asm_v_wasm.h"
            , "ast_utils.h"
            , "binaryen-c.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "parsing.h"
            , "pass.h"
            , "shared-constants.h"
            , "shell-interface.h"
            , "wasm-binary.h"
            , "wasm-builder.h"
            , "wasm-interpreter.h"
            , "wasm-printing.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm-validator.h"
            , "wasm.h"
            , "asmjs" </> "shared-constants.h"
            , "cfg" </> "Relooper.h"
            , "emscripten-optimizer" </> "istring.h"
            , "emscripten-optimizer" </> "optimizer.h"
            , "emscripten-optimizer" </> "parser.h"
            , "emscripten-optimizer" </> "simple_ast.h"
            , "emscripten-optimizer" </> "snprintf.h"
            , "support" </> "bits.h"
            , "support" </> "colors.h"
            , "support" </> "name.h"
            , "support" </> "safe_integer.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "asmjs" </> "asm_v_wasm.cpp"
          , [ "asm_v_wasm.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "emscripten-optimizer" </> "optimizer.h"
            , "emscripten-optimizer" </> "parser.h"
            , "emscripten-optimizer" </> "simple_ast.h"
            , "emscripten-optimizer" </> "snprintf.h"
            , "support" </> "name.h"
            , "support" </> "safe_integer.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "asmjs" </> "shared-constants.cpp"
          , [ "asmjs" </> "shared-constants.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "threads.h"
            ])
        , ( "ast" </> "ExpressionAnalyzer.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "hash.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "ast" </> "ExpressionManipulator.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "hash.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "cfg" </> "Relooper.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "parsing.h"
            , "pass.h"
            , "shared-constants.h"
            , "wasm-builder.h"
            , "wasm-printing.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "asmjs" </> "shared-constants.h"
            , "cfg" </> "Relooper.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "colors.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "CoalesceLocals.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "cfg" </> "cfg-traversal.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "learning.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "CodePushing.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "DeadCodeElimination.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "DuplicateFunctionElimination.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "hash.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "ExtractFunction.cpp"
          , [ "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "Inlining.cpp"
          , [ "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "parsing.h"
            , "pass.h"
            , "shared-constants.h"
            , "wasm-builder.h"
            , "wasm-printing.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "asmjs" </> "shared-constants.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "colors.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "LegalizeJSInterface.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "LocalCSE.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "ast" </> "hashed.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "hash.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "LogExecution.cpp"
          , [ "asm_v_wasm.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "shared-constants.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "asmjs" </> "shared-constants.h"
            , "emscripten-optimizer" </> "istring.h"
            , "emscripten-optimizer" </> "optimizer.h"
            , "emscripten-optimizer" </> "parser.h"
            , "emscripten-optimizer" </> "simple_ast.h"
            , "emscripten-optimizer" </> "snprintf.h"
            , "support" </> "name.h"
            , "support" </> "safe_integer.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "MemoryPacking.cpp"
          , [ "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "MergeBlocks.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "Metrics.cpp"
          , [ "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "colors.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "NameList.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "NameManager.cpp"
          , [ "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "OptimizeInstructions.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "parsing.h"
            , "pass.h"
            , "shared-constants.h"
            , "wasm-builder.h"
            , "wasm-printing.h"
            , "wasm-s-parser.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "asmjs" </> "shared-constants.h"
            , "ast" </> "bits.h"
            , "ast" </> "cost.h"
            , "ast" </> "properties.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "bits.h"
            , "support" </> "colors.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "PickLoadSigns.cpp"
          , [ "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "ast" </> "bits.h"
            , "ast" </> "properties.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "bits.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "PostEmscripten.cpp"
          , [ "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "asmjs" </> "shared-constants.h"
            , "ast" </> "localize.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "Precompute.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-interpreter.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "bits.h"
            , "support" </> "name.h"
            , "support" </> "safe_integer.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "Print.cpp"
          , [ "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "pretty_printing.h"
            , "wasm-printing.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "colors.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "PrintCallGraph.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "RelooperJumpThreading.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "RemoveImports.cpp"
          , [ "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "RemoveMemory.cpp"
          , [ "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "RemoveUnusedBrs.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "RemoveUnusedModuleElements.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "RemoveUnusedNames.cpp"
          , [ "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "ReorderFunctions.cpp"
          , [ "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "ReorderLocals.cpp"
          , [ "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "SimplifyLocals.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "ast" </> "count.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "Vacuum.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "passes" </> "pass.cpp"
          , [ "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-printing.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm-validator.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "passes" </> "passes.h"
            , "support" </> "colors.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "support" </> "archive.cpp"
          , [ "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "archive.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "support" </> "bits.cpp"
          , ["compiler-support.h", "support" </> "bits.h"])
        , ("support" </> "colors.cpp", ["support" </> "colors.h"])
        , ( "support" </> "command-line.cpp"
          , [ "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "command-line.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ("support" </> "file.cpp", ["support" </> "file.h"])
        , ("support" </> "safe_integer.cpp", ["support" </> "safe_integer.h"])
        , ( "support" </> "threads.cpp"
          , [ "compiler-support.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "wasm" </> "literal.cpp"
          , [ "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pretty_printing.h"
            , "wasm-type.h"
            , "emscripten-optimizer" </> "istring.h"
            , "emscripten-optimizer" </> "parser.h"
            , "emscripten-optimizer" </> "simple_ast.h"
            , "emscripten-optimizer" </> "snprintf.h"
            , "support" </> "bits.h"
            , "support" </> "colors.h"
            , "support" </> "safe_integer.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "wasm" </> "wasm-binary.cpp"
          , [ "asm_v_wasm.h"
            , "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "parsing.h"
            , "pass.h"
            , "shared-constants.h"
            , "wasm-binary.h"
            , "wasm-builder.h"
            , "wasm-printing.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm-validator.h"
            , "wasm.h"
            , "asmjs" </> "shared-constants.h"
            , "emscripten-optimizer" </> "istring.h"
            , "emscripten-optimizer" </> "optimizer.h"
            , "emscripten-optimizer" </> "parser.h"
            , "emscripten-optimizer" </> "simple_ast.h"
            , "emscripten-optimizer" </> "snprintf.h"
            , "support" </> "bits.h"
            , "support" </> "colors.h"
            , "support" </> "name.h"
            , "support" </> "safe_integer.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "wasm" </> "wasm-io.cpp"
          , [ "asm_v_wasm.h"
            , "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "parsing.h"
            , "pass.h"
            , "shared-constants.h"
            , "wasm-binary.h"
            , "wasm-builder.h"
            , "wasm-io.h"
            , "wasm-printing.h"
            , "wasm-s-parser.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm-validator.h"
            , "wasm.h"
            , "asmjs" </> "shared-constants.h"
            , "emscripten-optimizer" </> "istring.h"
            , "emscripten-optimizer" </> "optimizer.h"
            , "emscripten-optimizer" </> "parser.h"
            , "emscripten-optimizer" </> "simple_ast.h"
            , "emscripten-optimizer" </> "snprintf.h"
            , "support" </> "colors.h"
            , "support" </> "file.h"
            , "support" </> "name.h"
            , "support" </> "safe_integer.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ( "wasm" </> "wasm-s-parser.cpp"
          , [ "asm_v_wasm.h"
            , "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "parsing.h"
            , "pass.h"
            , "shared-constants.h"
            , "wasm-binary.h"
            , "wasm-builder.h"
            , "wasm-printing.h"
            , "wasm-s-parser.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm-validator.h"
            , "wasm.h"
            , "asmjs" </> "shared-constants.h"
            , "emscripten-optimizer" </> "istring.h"
            , "emscripten-optimizer" </> "optimizer.h"
            , "emscripten-optimizer" </> "parser.h"
            , "emscripten-optimizer" </> "simple_ast.h"
            , "emscripten-optimizer" </> "snprintf.h"
            , "support" </> "colors.h"
            , "support" </> "name.h"
            , "support" </> "safe_integer.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        , ("wasm" </> "wasm-type.cpp", ["compiler-support.h", "wasm-type.h"])
        , ( "wasm" </> "wasm.cpp"
          , [ "ast_utils.h"
            , "compiler-support.h"
            , "literal.h"
            , "mixed_arena.h"
            , "pass.h"
            , "wasm-builder.h"
            , "wasm-traversal.h"
            , "wasm-type.h"
            , "wasm.h"
            , "emscripten-optimizer" </> "istring.h"
            , "support" </> "name.h"
            , "support" </> "threads.h"
            , "support" </> "utilities.h"
            ])
        ]

main :: IO ()
main = do
    libdir <- System.Environment.getEnv "BINARYEN_LIBDIR"
    buildBinaryen libdir
    defaultMainWithHooks
        simpleUserHooks
        { preConf =
              \args flags ->
                  preConf
                      simpleUserHooks
                      args
                      flags
                      {configExtraLibDirs = libdir : configExtraLibDirs flags}
        }
