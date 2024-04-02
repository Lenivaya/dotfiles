_final: prev: {
  skippy-xd = prev.skippy-xd.overrideAttrs (_oa: {
    src = prev.fetchFromGitHub {
      owner = "felixfung";
      repo = "skippy-xd";
      rev = "2a4e6f5a73ed41067918e16737c380760b6200f1";
      hash = "sha256-G7bMnG0MJ6wu+87GCdPDoIfbBLyC87GB+pyBdZt3nS8=";
    };
  });
}
