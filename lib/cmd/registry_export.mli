(** Publish helper: writes the local layer cache, index, and source mirror into
    a tree an [oi] client expects from a remote registry. Driven by
    [oi build --export DIR]; no standalone command. *)

val run :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:D10.Config.clk ->
  sys:D10.Sysops.t ->
  os_key:string ->
  cache:Oi.Cache.t ->
  registry:string ->
  output:string ->
  unit
(** [run] writes the publishable registry tree under [output].

    Always written:
    - [<os_key>/<hash>.tar.zst] for every succeeded layer in the local cache
      (the binary tarballs clients restore via {!D10.Layer.pull_remote}).
    - [<os_key>/index.db] sqlite, populated with [layers] (including
      [tarball_sha256] / [tarball_size] columns), [layer_deps],
      [layer_binaries], [layer_meta]. No [layer_files] table — each layer's
      [layer.json] already lists what it ships, and per-file paths are 88% of
      the index.db on a typical export. {!oi search} doesn't need them.
    - [d10ir-archives/<sha>.tar.zst] for every consolidated source archive in
      [<cache>/d10ir/archives/] (baked by [oi repo bump]). The only
      source-tarball channel — [oi build] hard-errors when a needed sha isn't
      available locally and not on the remote. No opam source-mirror fallback.
    - [<os_key>/logs/manifest.json] + [<os_key>/audit.jsonl].

    Note: the cross-distro [handles/<handle>.md] report is not written here.
    Each export only sees its own oskey, so a per-distro write would race with
    parallel exports and [s3cmd put --skip-existing] would pin the first
    distro's incomplete view in S3. The handle report belongs in a server-side
    process that scans the merged bucket.

    Idempotent on repeat invocations. *)
