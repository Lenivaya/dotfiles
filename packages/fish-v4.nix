{
  cargo,
  cmake,
  fetchFromGitHub,
  gettext,
  pcre2,
  pkg-config,
  rustPlatform,
  rustc,
  stdenv,
  ...
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "fish";
  version = "4.0b1";

  src = fetchFromGitHub {
    owner = "fish-shell";
    repo = "fish-shell";
    rev = finalAttrs.version;
    hash = "sha256-O5xZHXNrJMpjTA2mrTqzMtU/55UArwoc2adc0R6pVl0=";
  };

  cargoDeps = rustPlatform.fetchCargoVendor {
    inherit (finalAttrs) src;
    hash = "sha256-jTVZKzX/Uy2RtyMbeQmatLLrOO+5S5jXrYKMGXNMcV4=";
  };

  env.FISH_BUILD_VERSION = finalAttrs.version;

  strictDeps = true;
  enableParallelBuilding = true;

  nativeBuildInputs = [
    pkg-config
    cargo
    cmake
    rustPlatform.cargoSetupHook
    rustc
  ];

  buildInputs = [
    pcre2
    gettext
  ];

  meta.mainProgram = "fish";
  passthru.shellPath = "/bin/fish";
})
