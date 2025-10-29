module Releases : sig
  include module type of struct include Ocaml_version.Releases end

  val maintenance : Ocaml_version.t -> Ocaml_version.t
  (** [maintenance version] returns the version of the branch tip on ocaml/ocaml
      for [version]. For example, [maintenance v4_08] is 4.08.2.

      @raise Not_found for unreleased versions *)
end = struct
  include Ocaml_version.Releases

  module Map = Map.Make(Ocaml_version)

  let trunks =
    let trunkify map v =
      let v =
        if Ocaml_version.major v = 3 && Ocaml_version.minor v = 7 then
          let patch =
            Option.fold ~none:0 ~some:int_of_string (Ocaml_version.extra v)
          in
          Ocaml_version.v ~patch 3 7
        else
          v
      in
      let patch = Option.map succ (Ocaml_version.patch v) in
      let v = Ocaml_version.with_just_major_and_minor v in
      Map.add v (Ocaml_version.with_patch v patch) map
    in
    List.fold_left trunkify Map.empty Ocaml_version.Releases.all

  let maintenance v =
    Map.find (Ocaml_version.with_just_major_and_minor v) trunks
end

(* Version comparison functions *)

(* Single release - compares major, minor and patch *)
let release ?not:exclude v _ version =
  let handle_3_07 v =
    match Ocaml_version.patch v with
    | None -> Option.map int_of_string (Ocaml_version.extra v)
    | (Some _) as v -> v
  in
  not (Option.fold ~none:false ~some:(Ocaml_version.equal version) exclude)
  && Ocaml_version.major v = Ocaml_version.major version
  && Ocaml_version.minor v = Ocaml_version.minor version
  && handle_3_07 v = handle_3_07 version

let exactly v _ version =
  Ocaml_version.compare v version = 0

(* Release series - compare major minor and must be a released version *)
let series v _ version =
  Ocaml_version.major v = Ocaml_version.major version
  && Ocaml_version.minor v = Ocaml_version.minor version
  && Ocaml_version.compare version (Releases.maintenance v) < 0

let gpr1330 v name version =
  let skip extra =
    List.mem extra ["statistical-memprof";
                    "bytecode-only";
                    "32bit";
                    "fp";
                    "fp+flambda"]
  in
  series v name version
  && not (Option.fold ~none:false ~some:skip (Ocaml_version.extra version))

let ocaml_variants = OpamPackage.Name.of_string "ocaml-variants"

let gcc10_releases v name version =
   release v name version
   && Ocaml_version.extra version <> Some "force-safe-string"
   && Ocaml_version.extra version <> Some "termux"

(* Complicated mix of mis-applied patches for 4.08 maintenance branch *)
let gcc10_trunk v name version =
  let some s =
    s = "force-safe-string" || String.ends_with ~suffix:"+force-safe-string" s
  in
  name = ocaml_variants
  && release Releases.(maintenance v) name version
  && not (Option.fold ~none:false ~some (Ocaml_version.extra version))

(* Non-variant versions of 4.09.0 *)
let released_4_09_0 name version =
  name <> ocaml_variants && release Releases.v4_09_0 name version

let variants_5_1_0 name version =
  name = ocaml_variants
  && series Releases.v5_1_0 name version
  && Ocaml_version.extra version <> Some "trunk"

let rescript v name version =
  name = ocaml_variants
  && release v name version
  && Ocaml_version.extra version = Some "rescript"

let commit_from ?(multicore=true) subject branch =
  let branch_name =
    Ocaml_version.to_string (Ocaml_version.with_just_major_and_minor branch)
  in
  let filter name version =
    let multicore =
      let is_multicore =
        Ocaml_version.extra version
        |> Option.map (fun s -> List.hd (String.split_on_char '+' s))
        |> Option.fold ~none:false ~some:(String.equal "domains")
      in
      not multicore && is_multicore
    in
    series branch name version && not multicore
  in
  filter, `Commit (branch_name, subject)

let clang_diff name version =
  let skip_4_01 extra =
    List.mem extra ["lsb"; "musl"; "musl+static"; "armv6-freebsd"]
  in
  let skip_4_00 extra =
    List.mem extra ["mirage-xen";
                    "mirage-unix";
                    "short-types";
                    "open-types";
                    "raspberrypi"]
  in
  let v4_00_1_debug_runtime =
    Ocaml_version.of_string_exn "4.00.1+debug-runtime"
  in
  let extra = Ocaml_version.extra version in
  series Releases.v4_01 name version
    && not (Option.fold ~none:false ~some:skip_4_01 extra)
  || (exactly v4_00_1_debug_runtime name version
      || exactly Releases.v4_00_1 name version)
      && not (Option.fold ~none:false ~some:skip_4_00 extra)

let v4_00_0_debug_runtime =
  Ocaml_version.of_string_exn "4.00.0+debug-runtime"
let v4_02_0_improved_errors =
  Ocaml_version.with_variant Releases.v4_02_0 (Some "improved-errors")
let v4_02_2_improved_errors =
  Ocaml_version.with_variant Releases.v4_02_2 (Some "improved-errors")
let v4_04_1_copatterns =
  Ocaml_version.with_variant Releases.v4_04_1 (Some "copatterns")

let series_3_09_except_metaocaml name version =
  series Releases.v3_09 name version
  && Ocaml_version.extra version <> Some "metaocaml"

let v3_09_1_except_metaocaml name version =
  release Releases.v3_09_1 name version
  && Ocaml_version.extra version <> Some "metaocaml"

(* Patches *)
let patches =
  [
    "ocaml-3.07-patch1.diffs", None, [
      release Releases.v3_07_1, `Patch "https://caml.inria.fr/pub/distrib/ocaml-3.07/ocaml-3.07-patch1.diffs";
    ];
    "ocaml-3.07-patch2.diffs", None, [
      release Releases.v3_07_2, `Patch "https://caml.inria.fr/pub/distrib/ocaml-3.07/ocaml-3.07-patch2.diffs";
    ];
    "pr4439.patch", None, [
      series_3_09_except_metaocaml, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/pr4439.patch";
    ];
    "ocamlopt-fPIC.patch", None, [
      series_3_09_except_metaocaml, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/ocamlopt-fPIC.patch";
    ];
    "pr4867.patch", None, [
      release Releases.v3_09_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/pr4867.patch.3.09.0";
      v3_09_1_except_metaocaml, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/pr4867.patch.3.09.1";
      release Releases.v3_09_2, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/pr4867.patch.3.09.2";
      release Releases.v3_09_3, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/pr4867.patch.3.09.3";
      release Releases.v3_10_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/pr4867.patch.3.10.0";
      release Releases.v3_10_1, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/pr4867.patch.3.10.1";
      release Releases.v3_10_2, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/pr4867.patch.3.10.2";
    ];
    "PIC.patch", None, [
      release Releases.v3_07_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/PIC.patch.3.07";
      release Releases.v3_07_1, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/PIC.patch.3.07+1";
      release Releases.v3_07_2, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/PIC.patch.3.07+2";
      release Releases.v3_08_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/PIC.patch.3.08.0";
      release Releases.v3_08_1, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/PIC.patch.3.08.1";
      release Releases.v3_08_2, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/PIC.patch.3.08.2";
      release Releases.v3_08_3, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/PIC.patch.3.08.3";
      release Releases.v3_08_4, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/PIC.patch.3.08.4";
      release Releases.v3_09_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/PIC.patch.3.09.0";
    ];
    "pr2061.patch", None, [
      series Releases.v3_07, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/pr2061.patch";
    ];
    "pr5237.patch", None, [
      series Releases.v3_11, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/pr5237.patch";
    ];
    (* XXX This is actually the same as the pr5237 backport to earlier
           versions *)
    "fix-binutils.patch", Some "os != \"win32\"", [
      release Releases.v3_12_0, `Patch "https://gist.githubusercontent.com/vicuna/864c7c8a5c03917ca1482d8fbba12d36/raw/fa7664cecc98d7d5d97ee6d8fb035c2e229bff57/0007-Fix-ocamlopt-w.r.t.-binutils-2.21.patch";
    ];
    "dc0776f55108a20dad5a9c06188545dc08dbf462.patch", None, [
      exactly (Ocaml_version.with_variant Releases.v4_00_1 (Some "raspberrypi")), `Patch "https://github.com/avsm/ocaml/commit/dc0776f55108a20dad5a9c06188545dc08dbf462.patch?full_index=1";
    ];
    "freebsd10-armv6-natdynlink.patch", None, [
      exactly (Ocaml_version.with_variant Releases.v4_01_0 (Some "armv6-freebsd")), `Patch "https://github.com/andrewray/mirage-fpga/releases/download/v0.1/freebsd10-armv6-natdynlink.patch";
    ];
    "bd7fa181cb64742c3b6cbb8ee13436554eb18cd7...fix-clang-build.diff", None, [
      clang_diff, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/fix-clang-build-ocaml-401.patch";
    ];
    "ocaml-4.06.1+termux.patch", None, [
      exactly (Ocaml_version.with_variant Releases.v4_06_1 (Some "termux")), `Patch "https://ygrek.org/files/ocaml-4.06.1+termux.patch";
    ];
    "fix-gcc10.patch", None, [
      release ~not:v4_00_0_debug_runtime Releases.v4_00_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/fix-gcc10.patch.4.00.0";
      exactly Releases.v4_00_1, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/fix-gcc10.patch.4.00.1";
      exactly Releases.v4_01_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/fix-gcc10.patch.4.01.0";
      exactly v4_02_0_improved_errors, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-variants/fix-gcc10.patch.4.02.0+improved-errors";
      release ~not:v4_02_0_improved_errors Releases.v4_02_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/fix-gcc10.patch.4.02.0";
      release Releases.v4_02_1, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/fix-gcc10.patch.4.02.1";
      exactly v4_02_2_improved_errors, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-variants/fix-gcc10.patch.4.02.2+improved-errors";
      release ~not:v4_02_2_improved_errors Releases.v4_02_2, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/fix-gcc10.patch.4.02.2";
      release Releases.v4_02_3, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/fix-gcc10.patch.4.02.3";
      release Releases.(maintenance v4_02), `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-variants/fix-gcc10.patch.4.02.4+trunk";
      release Releases.v4_03_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/fix-gcc10.patch.4.03.0";
      release Releases.(maintenance v4_03), `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-variants/fix-gcc10.patch.4.03.1+trunk";
      release Releases.v4_04_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/fix-gcc10.patch.4.04.0";
      exactly v4_04_1_copatterns, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-variants/fix-gcc10.patch.4.04.1+copatterns";
      release ~not:v4_04_1_copatterns Releases.v4_04_1, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/fix-gcc10.patch.4.04.1";
      release Releases.v4_04_2, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/fix-gcc10.patch.4.04.2";
      release Releases.(maintenance v4_04), `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-variants/fix-gcc10.patch.4.04.3+trunk";
      release Releases.v4_05_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/fix-gcc10.patch.4.05.0";
      release Releases.(maintenance v4_05), `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-variants/fix-gcc10.patch.4.05.1";
      gcc10_releases Releases.v4_06_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/fix-gcc10.patch.4.06.0";
      gcc10_releases Releases.v4_06_1, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/fix-gcc10.patch.4.06.1";
      gcc10_trunk Releases.v4_06, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-variants/fix-gcc10.patch.4.06.2";
      gcc10_releases Releases.v4_07_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/fix-gcc10.patch.4.07.0";
      release Releases.v4_07_1, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/fix-gcc10.patch.4.07.1";
      release Releases.(maintenance v4_07), `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-variants/fix-gcc10.patch.4.07.2";
      gcc10_releases Releases.v4_08_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/fix-gcc10.patch.4.08.0";
      gcc10_releases Releases.v4_08_1, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/fix-gcc10.patch.4.08.1";
      gcc10_trunk Releases.v4_08,`Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-variants/fix-gcc10.patch.4.08.2";
      released_4_09_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/fix-gcc10.patch.4.09.0";
    ];
    "improved-error.patch", None, [
      exactly v4_02_0_improved_errors, `Patch "https://gist.githubusercontent.com/andrewray/1928825fea090e50c0de/raw/e121b5cd176cdf6b882bc402276235b1c0a71b69/improved-error.patch";
    ];
    "add-conditional-compilation.patch", None, [
      rescript Releases.v4_06_1, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-variants/add-conditional-compilation.patch.4.06.1+rescript";
      rescript Releases.v4_10_2, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-variants/add-conditional-compilation.patch.4.10.2+rescript";
    ];
    "gpr1330.patch", None, [
      gpr1330 Releases.v4_02, `Commit ("4.02", "AArch64 GOT fixed");
      gpr1330 Releases.v4_03, `Commit ("4.03", "AArch64 GOT fixed");
      gpr1330 Releases.v4_04, `Commit ("4.04", "AArch64 GOT fixed");
      gpr1330 Releases.v4_05, `Commit ("4.05", "AArch64 GOT fixed");
    ];
    "alt-signal-stack.patch", None, [
      release Releases.v3_07_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/alt-signal-stack.patch.3.07";
      release Releases.v3_07_1, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/alt-signal-stack.patch.3.07+1";
      release Releases.v3_07_2, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/alt-signal-stack.patch.3.07+2";
      release Releases.v3_08_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/alt-signal-stack.patch.3.08.0";
      release Releases.v3_08_1, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/alt-signal-stack.patch.3.08.1";
      release Releases.v3_08_2, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/alt-signal-stack.patch.3.08.2";
      release Releases.v3_08_3, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/alt-signal-stack.patch.3.08.3";
      release Releases.v3_08_4, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/alt-signal-stack.patch.3.08.4";
      release Releases.v3_09_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/alt-signal-stack.patch.3.09.0";
      release Releases.v3_09_1, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/alt-signal-stack.patch.3.09.1";
      release Releases.v3_09_2, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/alt-signal-stack.patch.3.09.2";
      release Releases.v3_09_3, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/alt-signal-stack.patch.3.09.3";
      release Releases.v3_10_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/alt-signal-stack.patch.3.10.0";
      release Releases.v3_10_1, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/alt-signal-stack.patch.3.10.1";
      release Releases.v3_10_2, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/alt-signal-stack.patch.3.10.2";
      release Releases.v3_11_0, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/alt-signal-stack.patch.3.11.0";
      release Releases.v3_11_1, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/alt-signal-stack.patch.3.11.1";
      release Releases.v3_11_2, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/alt-signal-stack.patch.3.11.2";
    ];
    "alt-signal-stack.patch", None,
      List.map (commit_from ~multicore:false "Dynamically allocate the alternate signal stack") [
        Releases.v3_12;
        Releases.v4_00;
        Releases.v4_01;
        Releases.v4_02;
        Releases.v4_03;
        Releases.v4_04;
        Releases.v4_05;
        Releases.v4_06;
        Releases.v4_07;
        Releases.v4_08;
        Releases.v4_09;
        Releases.v4_10;
        Releases.v4_11;
        Releases.v4_12;
      ];
    "0001-Re-generate-configure.patch", None, [
      release Releases.v4_09_1, `Patch "https://raw.githubusercontent.com/ocaml/opam-source-archives/main/patches/ocaml-base-compiler/0001-Re-generate-configure.patch";
    ];
    "zstd-detection.patch", None, [
      variants_5_1_0, `Commit ("5.1", "Merge pull request #13100 from dra27/zstd-runs");
    ];
  ]

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

module Store = Git_unix.Store

open Lwt.Infix
let ( >>? ) = Lwt_result.bind
let ( >>! ) v msg = Lwt_result.map_error (fun _ -> msg) v

exception Error of [
  | `Store of string
  | `Not_found of Store.hash
  | `Reference_not_found of Git.Reference.t
  | `Download_error of string * int
  | `Missing_opam of string
  | `No_packages
]

let store_failure f = function
| Ok ok -> f ok
| Error (err : Store.error) ->
    match err with
    | `Msg msg -> Lwt.fail (Error (`Store msg))
    | `Not_found hash -> Lwt.fail (Error (`Not_found hash))
    | `Reference_not_found ref -> Lwt.fail (Error (`Reference_not_found ref))
    | _ -> Lwt.fail (Error (`Store "Unknown store error"))

let read_tree store hash =
  Store.read store hash >>= store_failure (function
    | Git.Value.Tree tree -> Lwt.return tree
    | _ -> Lwt.fail (Error (`Not_found hash)))

let read_commit store hash =
  Store.read store hash >>= store_failure (function
    | (Git.Value.Commit commit) -> Lwt.return commit
    | _ -> Lwt.fail (Error (`Not_found hash)))

let read_blob store hash =
  Store.read store hash >>= store_failure (function
    | Git.Value.Blob blob -> Lwt.return blob
    | _ -> Lwt.fail (Error (`Not_found hash)))

let resolve store ref =
  Store.Ref.resolve store ref >>= store_failure Lwt.return

let open_repository repo_dir =
  let path = Fpath.v repo_dir in
  let dotgit =
    let dotgit = Filename.concat repo_dir ".git" in
    if Sys.file_exists dotgit && Sys.is_directory dotgit then
      None
    else if Sys.file_exists repo_dir && Sys.is_directory repo_dir then
      Some path
    else
      failwith "Repository checkout not found"
  in
  Store.v ?dotgit path >>= store_failure Lwt.return

let rec search_for store subjects results commit_hash =
  read_commit store commit_hash >>= fun commit ->
    let subject =
      Store.Value.Commit.message commit
      |> Option.map (fun message ->
          String.index_opt message '\n'
          |> Option.map (fun index -> String.sub message 0 index)
          |> Option.value ~default:message)
    in
    let subjects, results =
      match subject with
      | Some subject when StringSet.mem subject subjects ->
          StringSet.remove subject subjects,
          StringMap.add subject commit_hash results
      | _ ->
          subjects, results
    in
    let parents = Store.Value.Commit.parents commit in
    if StringSet.is_empty subjects || parents = [] then
      Lwt.return results
    else
      search_for store subjects results (List.hd parents)

let find_commits store branch subjects =
  let ref = Git.Reference.v ("refs/remotes/upstream/" ^ branch) in
  resolve store ref
    >>= search_for store subjects StringMap.empty
    >>= fun subjects -> Lwt.return subjects

open Lwt.Syntax

let get_checksums =
  let rec get_checksums orig_url max_redirects url () =
    Cohttp_lwt_unix.Client.get (Uri.of_string url) >>= fun (response, body) ->
      let status = Cohttp.Response.status response in
      let code = Cohttp.Code.code_of_status status in
      let headers = Http.Response.headers response in
      let location = Http.Header.get headers "location" in
      match status with
      | `OK ->
          Cohttp_lwt.Body.to_string body >|= fun body ->
            let md5 =
              Digestif.MD5.to_hex (Digestif.MD5.digest_string body)
            in
            let sha256 =
              Digestif.SHA256.to_hex (Digestif.SHA256.digest_string body)
            in
            ~md5, ~sha256
      | `Permanent_redirect
      | `Moved_permanently
      | `Found
      | `Temporary_redirect when max_redirects > 0 && location <> None ->
          let* () = Cohttp_lwt.Body.drain_body body in
          get_checksums orig_url (pred max_redirects) (Option.get location) ()
      | _ ->
          let* () = Cohttp_lwt.Body.drain_body body in
          Lwt.fail (Error (`Download_error (orig_url, code)))
  in
  let download_pool = Lwt_pool.create 10 (fun () -> Lwt.return_unit) in
  let (known : (string, (md5:string * sha256:string) Lwt.t) Hashtbl.t) =
    Hashtbl.create 63
  in
  fun ?(md5=false) ?(sha256=true) url ->
    let+ (~md5:md5_hash, ~sha256:sha256_hash) =
      try Hashtbl.find known url
      with Not_found ->
        let r = Lwt_pool.use download_pool (get_checksums url 10 url) in
        Hashtbl.add known url r; r
    in
    let md5 = if md5 then Some md5_hash else None in
    let sha256 = if sha256 then Some sha256_hash else None in
    ~md5, ~sha256

(* Processing *)
let patches =
  let get_commits commits (_, _, versions) =
    let f commits (_, source) =
      match source with
      | `Commit (branch, subject) ->
          let subjects =
            StringMap.find_opt branch commits
            |> Option.value ~default:StringSet.empty
          in
          StringMap.add branch (StringSet.add subject subjects) commits
      | `Patch _ ->
          commits
    in
    List.fold_left f commits versions
  in
  let commits =
    let commits = List.fold_left get_commits StringMap.empty patches in
    open_repository "ocaml" >|= fun store ->
      StringMap.mapi (find_commits store) commits
  in
  let patches =
    let combine patch patch_filter (filter, source) =
      let filter_of_string s =
        let value = OpamParser.FullPos.value_from_string s "<filter>" in
        OpamPp.parse OpamFormat.V.filter ~pos:value.pos [value]
      in
      let patch_filter = Option.map filter_of_string patch_filter in
      match source with
      | `Patch url ->
          let checksums = get_checksums ~md5:true url in
          filter, patch, patch_filter, Lwt.return url, checksums
      | `Commit (branch, subject) ->
          let url =
            let* commits in
            StringMap.find branch commits >|= fun subjects ->
              Printf.sprintf
                "https://github.com/ocaml/ocaml/commit/%s.patch?full_index=1"
                (Store.Hash.to_hex (StringMap.find subject subjects))
          in
          let checksums =
            let* url in
            get_checksums url
          in
          filter, patch, patch_filter, url, checksums
    in
    patches
    |> List.map (fun (patch, filter, versions) ->
                   List.map (combine patch filter) versions)
    |> List.flatten
  in
  fun nv ->
    let name = OpamPackage.name nv in
    let version =
      OpamPackage.version nv
      |> OpamPackage.Version.to_string
      |> Ocaml_version.of_string_exn
    in
    let f (filter, patch, patch_filter, url, checksums) =
      if filter name version then
        Some (patch, patch_filter, url, checksums)
      else
        None
    in
    List.filter_map f patches

let packages = StringSet.of_list [
  "ocaml-base-compiler"; "ocaml-compiler"; "ocaml-variants"
]

let third_party = OpamPackage.Set.of_list (List.map OpamPackage.of_string [
  (* OxCaml *)
  "ocaml-variants.5.1.1+flambda2";
  "ocaml-variants.5.1.1+flambda2+trunk";
  (* Lost patches *)
  "ocaml-variants.4.00.1+french";
  "ocaml-variants.4.00.1+annot";
  "ocaml-variants.4.00.0+fp";
])

let check_opam_file name opam =
  let nv = OpamPackage.of_string name in
  let is_prerelease =
    let v =
      OpamPackage.version nv
      |> OpamPackage.Version.to_string
      |> Ocaml_version.of_string_exn
    in
    let extra =
      Ocaml_version.extra v
      |> Option.map (fun s -> List.hd (String.split_on_char '+' s))
      |> Option.value ~default:""
    in
    Ocaml_version.prerelease v <> None
    || String.starts_with ~prefix:"alpha" extra
    || String.starts_with ~prefix:"beta" extra
    || String.starts_with ~prefix:"rc" extra
  in
  let empty = StringSet.of_list [
    (* Unclear why this is skipped *)
    "ocaml-variants.4.04.0+trunk+forced_lto";
    (* Disabled in opam-repository-archive *)
    "ocaml-variants.4.04.0+copatterns";
  ] in
  let expected_patches =
    if is_prerelease || StringSet.mem name empty then
      []
    else
      patches nv
  in
  (* Apply various fudges *)
  (* XXX With the hope that they can be removed... *)
  let expected_patches =
    match name with
    | "ocaml-variants.4.02.2+improved-errors"
    | "ocaml-variants.4.02.3+buckle-master" ->
        (* XXX gpr1330 patch is last *)
        let a, b =
          let f (name, _, _, _) = name <> "gpr1330.patch" in
          List.partition f expected_patches
        in
        a @ b
    | "ocaml-variants.4.02.3+PIC" ->
        (* XXX Uses the 4.03 version of gpr1330! *)
        let gpr1330 =
          let patches =
            patches (OpamPackage.of_string "ocaml-base-compiler.4.03.0")
          in
          List.find (fun (name, _, _, _) -> name = "gpr1330.patch") patches
        in
        List.map (fun ((name, _, _, _) as patch) ->
                    if name = "gpr1330.patch" then
                      gpr1330
                    else
                      patch) expected_patches
    | _ ->
        expected_patches
  in
  let patches = OpamFile.OPAM.patches opam in
  if not (OpamPackage.Set.mem nv third_party) && not is_prerelease then begin
    let f (name, filter, url, checksums) =
      let+ url
      and+ ~md5, ~sha256 = checksums in
      name, filter, None, Some url, md5, sha256, None
    in
    let+ expected_patches = Lwt_list.map_p f expected_patches in
    let extra_files =
      Option.value ~default:[] (OpamFile.OPAM.extra_files opam)
      |> OpamFilename.Base.Map.of_list
    in
    let extra_sources =
      OpamFilename.Base.Map.of_list (OpamFile.OPAM.extra_sources opam)
    in
    let patches =
      let augment (patch, filter) =
        if OpamFilename.Base.Map.mem patch extra_files then
          let patch = OpamFilename.Base.to_string patch in
          patch, filter, Some name, None, None, None, None
        else match OpamFilename.Base.Map.find_opt patch extra_sources with
        | Some url ->
            let md5, sha256, sha512 =
              let f (md5, sha256, sha512) hash =
                let content = Some (OpamHash.contents hash) in
                match OpamHash.kind hash with
                | `MD5 -> (content, sha256, sha512)
                | `SHA256 -> (md5, content, sha512)
                | `SHA512 -> (md5, sha256, content)
              in
              List.fold_left f (None, None, None) (OpamFile.URL.checksum url)
            in
            let patch = OpamFilename.Base.to_string patch in
            let url = OpamUrl.to_string (OpamFile.URL.url url) in
            patch, filter, None, Some url, md5, sha256, sha512
        | None ->
            let patch = OpamFilename.Base.to_string patch in
            patch, filter, None, None, None, None, None
      in
      List.map augment patches
    in
    if patches <> expected_patches then begin
      Printf.printf "%s patches are INCONSISTENT\n" name;
      let display (name, filter, file, url, md5, sha256, sha512) =
        let display_hash name hash =
          Printf.printf "    with %s=%s\n" name hash
        in
        let filter = if filter = None then "" else "FILTERED " in
        let source =
          match file, url with
          | Some file, None -> "files/" ^ file
          | None, Some url -> url
          | Some _, Some _ -> "INCONSISTENT SOURCE"
          | None, None -> "UNKNOWN SOURCE"
        in
        Printf.printf "  - %s%s from %s\n" filter name source;
        Option.iter (display_hash "md5") md5;
        Option.iter (display_hash "sha256") sha256;
        Option.iter (display_hash "sha512") sha512
      in
      Printf.printf "  Expected:\n";
      List.iter display expected_patches;
      Printf.printf "  Got:\n";
      List.iter display patches
    end
  end else
    Lwt.return_unit

let check_opam_file name content =
  let opam =
    try Some (OpamFile.OPAM.read_from_string content)
    with _ -> None
  in
  (* XXX Better logging, etc. *)
  if opam = None then
    Printf.eprintf "Unable to parse opam file for %s\n" name;
  Option.fold ~none:Lwt.return_unit ~some:(check_opam_file name) opam

let analyse_tree store name tree_hash =
  let package_prefix = name ^ "." in
  read_tree store tree_hash >>= fun tree ->
    let f {Git.Tree.name; perm; node} =
      if perm = `Dir && String.starts_with ~prefix:package_prefix name then
          read_tree store node >>= fun tree ->
            let f {Git.Tree.name; perm; node} =
              if name = "opam" && perm = `Normal then
                Some node
              else
                None
            in
            match List.find_map f (Store.Value.Tree.to_list tree) with
            | None ->
                Lwt.fail (Error (`Missing_opam name))
            | Some node ->
                read_blob store node >>= fun content ->
                  check_opam_file name (Store.Value.Blob.to_string content)
      else
        Lwt.return_unit
    in
    Lwt_list.iter_p f (Store.Value.Tree.to_list tree)

let get_packages_tree branch store =
  let ref = Git.Reference.v ("refs/remotes/upstream/" ^ branch) in
  resolve store ref >>= fun hash ->
    read_commit store hash >>= fun commit ->
      read_tree store (Store.Value.Commit.tree commit) >>= fun tree ->
        let entries = Store.Value.Tree.to_list tree in
        let f = function
        | {Git.Tree.name; perm = `Dir; node} when name = "packages" ->
            Some node
        | _ ->
            None
        in
        match List.find_map f entries with
        | None ->
            Lwt.fail (Error `No_packages)
        | Some tree_hash ->
            read_tree store tree_hash >>= fun tree ->
              let f {Git.Tree.name; perm; node} =
                if perm = `Dir && StringSet.mem name packages then
                  analyse_tree store name node
                else
                  Lwt.return_unit
              in
              Lwt_list.iter_p f (Store.Value.Tree.to_list tree)

let main () =
  (* XXX Parallel iter over list! *)
  let+ () =
    open_repository "opam-repository" >>= get_packages_tree "master"
  and+ () =
    open_repository "opam-repository-archive" >>= get_packages_tree "main"
  in
  ()

let () =
  Lwt_main.run begin
    Lwt.catch
      main
      (function
      | Error (`Store msg) ->
          Lwt_io.eprintf "Error from the git store: %s\n" msg
      | Error (`Not_found hash) ->
          Lwt_io.eprintf "Could not retrieve %s from the store\n"
            (Store.Hash.to_hex hash)
      | Error (`Reference_not_found ref) ->
          Lwt_io.eprintf "Unable to resolve %s\n"
            (Git.Reference.to_string ref)
      | Error (`Download_error (url, code)) ->
          Lwt_io.eprintf "Got code %d downloading %s\n" code url
      | Error (`Missing_opam nv) ->
          Lwt_io.eprintf "opam file not found for %s\n" nv
      | Error `No_packages ->
          Lwt_io.eprintf "Couldn't open packages/ in opam-repository!\n"
      | _ ->
          Lwt.return_unit)
  end
