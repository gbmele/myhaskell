{ pkgs }: {
    deps = [
        pkgs.haskellPackages.concurrent-dns-cache
        (pkgs.haskellPackages.ghcWithPackages (pkgs: [
            # Put your dependencies here!
        ]))
        pkgs.haskell-language-server
    ];
}