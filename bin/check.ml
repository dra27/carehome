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
      List.iter display patches;
    end;
    Some (name, opam)
  end else
    Lwt.return None

let check_opam_file name content =
  let opam =
    try Some (OpamFile.OPAM.read_from_string content)
    with _ -> None
  in
  (* XXX Better logging, etc. *)
  if opam = None then
    Printf.eprintf "Unable to parse opam file for %s\n" name;
  Option.fold ~none:(Lwt.return None) ~some:(check_opam_file name) opam

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
                  let* test =
                    check_opam_file name (Store.Value.Blob.to_string content)
                  in
                  Lwt.return test
      else
        Lwt.return None
    in
    Lwt_list.filter_map_p f (Store.Value.Tree.to_list tree)

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
                  let* packages = analyse_tree store name node in
                  Lwt.return (Some packages)
                else
                  Lwt.return None
              in
              Lwt_list.filter_map_p f (Store.Value.Tree.to_list tree)
                >|= List.flatten

let distros =
  let latest_only prev distro =
    if Dockerfile_opam.Distro.is_same_distro prev distro then
      prev, None
    else
      distro, Some distro
  in
  Dockerfile_opam.Distro.active_distros `X86_64
  |> List.filter (fun d -> Dockerfile_opam.Distro.os_family_of_distro d = `Linux)
  |> List.sort (fun l r -> -(Dockerfile_opam.Distro.compare l r))
  |> List.fold_left_map latest_only (`Cygwin `Latest)
  |> snd
  |> List.filter_map Fun.id
  |> List.sort Dockerfile_opam.Distro.compare
  |> List.map Dockerfile_opam.Distro.resolve_alias

let base_image ic repo_sha archive_sha arch (distro : Dockerfile_opam.Distro.distro) =
  let shims =
    if distro = `Debian `V10 then
      {|
  sudo sed -i -e 's/deb\./archive./' /etc/apt/sources.list|}
    else if distro = `CentOS `V8 then
      {|
  sudo sed -i -e '/^mirrorlist/s/^/#/' \
              -e 's|^#baseurl=http://mirror\.centos\.org|baseurl=http://vault.centos.org|' /etc/yum.repos.d/CentOS-*|}
    else
      ""
  in
  let patch =
    if distro = `Debian `V10 then
      {|
  cd /tmp
  # We need the modernisations reported in Patch 2.8 for various undefined
  # behaviour. The alt-signal-stack patch will run out of memory when building
  # in buildkit (but not in docker run or with the legacy builder!)
  curl -LO https://mirrorservice.org/sites/ftp.gnu.org/gnu/patch/patch-2.8.tar.gz
  tar -xf patch-2.8.tar.gz
  cd patch-2.8
  ./configure --disable-year2038
  make
  sudo make install|}
    else
      ""
  in
  let platform, key =
    match arch with
    | `X86_64 -> "", ""
    | `I386 -> "--platform=linux/i386 ", "32bit-"
  in
  let extra_tweaks =
    match distro with
    | `CentOS (`V10)
    | `Fedora (`V41 | `V43)
    | `OracleLinux (`V10) ->
        {|
USER root
RUN sed -i -e '/auth/s/include.*/sufficient pam_permit.so/' /etc/pam.d/sudo
USER opam|}
    | _ ->
        ""
  in
  let extra_packages =
    match distro with
    | `Alpine _ ->
        {|
  sudo apk add openssl|}
    | `CentOS _
    | `Fedora _
    | `OracleLinux _ ->
        {|
  sudo dnf install -y openssl|}
    | _ ->
        ""
  in
  let remote =
    if distro = `Alpine `V3_12 then
      {|
  git -C opam-repository remote set-url origin https://github.com/ocaml/opam-repository.git|}
    else
      ""
  in
  let distro = Dockerfile_opam.Distro.tag_of_distro (distro :> Dockerfile_opam.Distro.t) in
  Printf.fprintf ic {|
FROM %socaml/opam:%s-opam AS %s-%s6693-opam%s
RUN <<End-of-Script%s
  git clone https://github.com/dra27/opam.git
  cd opam
  git checkout 6693-2.4.1
  make cold
End-of-Script
FROM %socaml/opam:%s-opam AS %s-%sbase
ENV OPAMYES="1" OPAMCONFIRMLEVEL="unsafe-yes" OPAMERRLOGLEN="0" OPAMPRECISETRACKING="1"
COPY --from=%s-%s6693-opam /home/opam/opam/opam /usr/bin/opam
RUN <<End-of-Script%s
  git -C opam-repository fetch origin
  git -C opam-repository checkout %s
  git clone https://github.com/ocaml/opam-repository-archive.git
  git -C opam-repository-archive checkout %s
  opam update%s
  opam update --depexts
  opam repo add --set-default --rank=2 archive opam-repository-archive%s
  rm -rf /home/opam/.opam/download-cache
  touch results
End-of-Script
COPY --from=cache --chown=opam:opam /home/opam/.opam/download-cache /home/opam/.opam/download-cache
|} platform distro distro key extra_tweaks extra_packages platform distro distro key distro key remote repo_sha archive_sha shims patch

(* XXX Should be memoized - this gets calculated for each dockerfile for no reason *)
let create_cache_stage oc repo_sha archive_sha packages excludes builds =
  (* XXX Use Dockerfile commands (maybe?) *)
  (* XXX The cache builder could be shared with the general "make builders" infra *)
(*
  Printf.printf "Creating Dockerfile.prime\n%!";
  Out_channel.with_open_text "Dockerfile.prime" @@ fun oc ->
*)
    Printf.fprintf oc {|FROM ocaml/opam:ubuntu-24.04-opam AS environment
ENV OPAMYES="1" OPAMCONFIRMLEVEL="unsafe-yes" OPAMERRLOGLEN="0" OPAMPRECISETRACKING="1"
RUN <<End-of-Script
  git -C opam-repository fetch origin
  git -C opam-repository checkout %s
  git clone https://github.com/ocaml/opam-repository-archive.git
  git -C opam-repository-archive checkout %s
  sudo ln -f /usr/bin/opam-2.4 /usr/bin/opam
  opam update
  opam repo add --set-default --rank=2 archive opam-repository-archive
  mkdir t
  cd t
  for v in $(seq 0 3); do
    opam source ocaml-config.$v
  done
End-of-Script
FROM environment AS cache
RUN <<End-of-Script
  cd t
|} repo_sha archive_sha;
    let _ =
      let source seen (name, opam) =
        if StringMap.mem name excludes then
          seen
        else
          let urls =
            List.filter_map (fun (_, url) -> if OpamFile.URL.checksum url = [] then None else Some (OpamUrl.to_string (OpamFile.URL.url url))) (OpamFile.OPAM.extra_sources opam)
          in
          let urls =
            Option.map (fun url -> if OpamFile.URL.checksum url = [] then urls else (OpamUrl.to_string (OpamFile.URL.url url))::urls) (OpamFile.OPAM.url opam)
            |> Option.value ~default:urls
            |> StringSet.of_list
          in
          if not (StringSet.is_empty (StringSet.diff urls seen)) then
            let () = Printf.fprintf oc "  opam source %s\n" name in
            StringSet.union urls seen
          else
            seen
      in
      List.fold_left source StringSet.empty (List.sort Stdlib.compare packages)
    in
    Printf.fprintf oc {|  cd ..
  rm -rf t
  echo Epoch1>epoch
End-of-Script
#RUN --mount=type=cache,id=carehome-download-cache,uid=1000,gid=1000,target=/home/opam/shared-cache <<End-of-Script
#  rm -rf /home/opam/shared-cache/*
#  cp -a /home/opam/.opam/download-cache/* /home/opam/shared-cache/
#End-of-Script
|};
    let f bases (_, arch, (distro : Dockerfile_opam.Distro.distro), _, _, _) =
      let key =
        Dockerfile_opam.Distro.tag_of_distro (distro :> Dockerfile_opam.Distro.t) ^ (if arch = `I386 then "-32bit" else "") ^ "-base"
      in
      if StringSet.mem key bases then
        bases
      else
        let () = base_image oc repo_sha archive_sha arch distro in
        StringSet.add key bases
    in
    List.fold_left f StringSet.empty builds
(*
    Printf.fprintf oc {|
FROM ocaml/opam:ubuntu-25.10-opam AS collect
RUN --mount=from=cache,src=/home/opam,dst=/mnt/opam-cache \
|};
    let _ =
      List.fold_left (fun bases (_, arch, distro, _, _, _) ->
        let key = Ocaml_version.string_of_arch (arch : [`X86_64 | `I386] :> Ocaml_version.arch) ^ "-" ^ Dockerfile_opam.Distro.human_readable_string_of_distro (distro :> Dockerfile_opam.Distro.t) in
        if StringSet.mem key bases then
          bases
        else
          let syskey = if arch = `I386 then "32bit-" else "" in
          let distro = Dockerfile_opam.Distro.tag_of_distro (distro :> Dockerfile_opam.Distro.t) in
          let () = Printf.fprintf oc "    --mount=from=%s-%sbase,src=/home/opam,dst=/mnt/opam-%s-%sbase \\\n" distro syskey distro syskey in
          StringSet.add key bases) StringSet.empty builds
    in
    Printf.fprintf oc {|    cat /mnt/opam-*/epoch > /home/opam/results
|}
*)

(*
let opam_version_in_distro = function
| `Debian `V10 -> "2.2"
| `Ubuntu `V20_04 -> "2.3"
| _ -> "2.4"
*)

let is_slow packages =
  List.exists
    (fun elt -> let elt = String.lowercase_ascii elt in elt = "ber" || elt = "flambda" || elt = "forced_lto")
    (String.split_on_char '+' (List.hd packages))

let build_package oc ~slow ~pin filter _repo_sha _archive_sha borked ((previous, bases) as acc) (stage_name, arch, (distro : Dockerfile_opam.Distro.distro), _, repo, packages) =
  let is_slow = is_slow packages in
        let name = List.hd packages in
        let _version =
          OpamPackage.of_string name
          |> OpamPackage.version
          |> OpamPackage.Version.to_string
          |> Ocaml_version.of_string_exn
        in
  (* Don't build the package at all if this we're building the slow Dockerfile (slow = true)
     and this package is _not_ slow. The slow = false Dockerfile always contains everything
     for the distro. *)
  if not (filter distro) || slow && not is_slow (*|| Ocaml_version.major _version < 5*) then
    acc
  else
    let key =
      Dockerfile_opam.Distro.tag_of_distro (distro :> Dockerfile_opam.Distro.t) ^ (if arch = `I386 then "-32bit" else "") ^ "-base"
    in
    let bases = StringSet.remove key bases in
(*
      if StringSet.mem key bases then
        bases
      else
        (*let () = base_image oc repo_sha archive_sha arch distro in*)
        StringSet.add key bases
    in
*)
    let platform =
      let distro = Dockerfile_opam.Distro.tag_of_distro (distro :> Dockerfile_opam.Distro.t) in
      if arch = `I386 then
        "--platform=linux/i386 " ^ distro ^ "-32bit"
      else
        distro
    in
    let repo =
      Option.fold ~none:"" ~some:(fun r -> "--repos=" ^ r ^ " ") repo
    in
    let packages =
      match packages with
      | [name] -> name
      | packages -> "--packages=" ^ String.concat "," packages
    in
    let mount_previous, get_previous, next =
      if StringMap.mem name borked then
        "", "", previous
      else
        let next = Some ("test-" ^ stage_name) in
        Option.map (fun previous -> "--mount=from=" ^ previous ^ ",src=/home/home,dst=/mnt/previous ", "\n  cp /mnt/previous/results /home/opam/results", next) previous
        |> Option.value ~default:("", "", next)
    in
    Printf.fprintf oc {|
FROM %s-base AS test-%s%s
#COPY --from=collect /home/opam/results /home/opam/prime-lock
#RUN --mount=type=cache,id=carehome-download-cache,uid=1000,gid=1000,target=/home/opam/shared-cache <<End-of-Script
#  rm -rf /home/opam/.opam/download-cache/*
#  mkdir -p /home/opam/.opam/download-cache
#  cp -a /home/opam/shared-cache/* /home/opam/.opam/download-cache/
#End-of-Script
RUN <<End-of-Script%s
mkdir -p /home/opam/logs
for i in $(seq 2 -1 0); do
  if opam switch create -v ocaml %s%s > /home/opam/logs/build-$i 2>&1; then
    rm -f /home/opam/logs/build-$i
    break
  else
    cat /home/opam/logs/build-$i
  fi
done
test ! -e /home/opam/logs/build-0
End-of-Script
RUN %s<<End-of-Script%s
  opam exec -- ocamlopt -v | head -n 1 >> /home/opam/results
End-of-Script
|} platform stage_name (if pin then "\nCOPY --from=pin /home/opam/results /home/opam/results" else "") (if pin then "\nrm -f /home/opam/results" else "") repo packages mount_previous get_previous;
    (next, bases)

let emit_stage ?(slow = false) ?(pin = false) filter name opam_repository_sha opam_repository_archive_sha borked excludes packages builds =
  let filename = "Dockerfile." ^ name ^ (if slow then "-slow" else "") in
  Printf.printf "Creating %s\n%!" filename;
  Out_channel.with_open_text filename @@ fun oc ->
    let bases = create_cache_stage oc opam_repository_sha opam_repository_archive_sha packages excludes builds in
    let (next, bases) = List.fold_left (build_package oc ~slow ~pin filter opam_repository_sha opam_repository_archive_sha borked) (None, bases) builds in
    Printf.fprintf oc {|
FROM ocaml/opam:ubuntu-25.10-opam AS collect
RUN --mount=from=%s,src=/home/opam,dst=/mnt/opam-last-build \|} (Option.get next);
  StringSet.iter (fun base -> Printf.fprintf oc {|
    --mount=from=%s,src=/home/opam,dst=/mnt/opam-%s \|} base base) bases;
  Printf.fprintf oc {|
  cat /mnt/opam-*/results > /home/opam/results
|}
(*
    let _ =
      List.fold_left (fun prefix (stage_name, _, distro, _, _, packages) ->
        let name = List.hd packages in
        let _version =
          OpamPackage.of_string name
          |> OpamPackage.version
          |> OpamPackage.Version.to_string
          |> Ocaml_version.of_string_exn
        in
        if (*Ocaml_version.major _version < 5 ||*) not (filter distro) || slow && not (is_slow packages) || (*not (List.exists ((=) "musl") (String.split_on_char '+' name)) || *)StringMap.mem name borked (*|| StringSet.mem name musl_clang*) then
          prefix
        else
          let () = Printf.fprintf oc "%s --mount=from=test-%s,src=/home/opam,dst=/mnt/opam-%s \\\n" prefix stage_name stage_name in
          "   ") "RUN" builds
    in
    Printf.fprintf oc {|    cat /mnt/opam-*/vnum > /home/opam/results
|}
*)

let get_packages () =
  let+ main_repo =
    open_repository "opam-repository" >>= get_packages_tree "master"
  and+ archive_repo =
    open_repository "opam-repository-archive" >>= get_packages_tree "main"
  in
  main_repo @ archive_repo

let generate_test_files opam_repository_sha opam_repository_archive_sha excludes packages =
  (* Packages which don't appear to be working properly - these get excluded at
     collection time *)
  let borked = StringMap.of_list [
    (* XXX This has appeared possibly since switching the build to use opam 2.4.1? *)
    "ocaml-variants.4.04.0+BER", "Needs investigating further";
  ] in
  (* Packages which need a 32bit container (these are a *)
  let requires_32bit compiler packages =
    List.mem "ocaml-option-32bit" packages
    || String.ends_with ~suffix:"+32bit" compiler
  in
  (* Packages build with musl and clang *)
  let musl_clang = StringSet.of_list [
    "ocaml-variants.4.05.0+musl+flambda"; (* musl-clang *)
    "ocaml-variants.4.05.0+musl+static+flambda"; (* musl-clang *)
    "ocaml-variants.4.06.0+musl+flambda"; (* musl-clang *)
    "ocaml-variants.4.06.0+musl+static+flambda"; (* musl-clang *)
    "ocaml-variants.4.06.1+musl+flambda"; (* musl-clang *)
    "ocaml-variants.4.06.1+musl+static+flambda"; (* musl-clang *)
  ] in
  (* Packages which require musl patches at the moment (= all of them) *)
  let is_musl_package compiler packages =
    let uses_option_package = function
    | "ocaml-option-static" | "ocaml-option-musl" -> true
    | _ -> false
    in
    List.exists uses_option_package packages
    || List.mem "musl" (String.split_on_char '+' compiler)
  in
  (* Packages missing gcc10 patches *)
  let gcc10 = StringSet.of_list [
    "ocaml-variants.4.00.0+debug-runtime";
    "ocaml-variants.4.00.1+BER";
    "ocaml-variants.4.00.1+debug-runtime";
    "ocaml-variants.4.00.1+mirage-unix";
    "ocaml-variants.4.00.1+mirage-xen";
    "ocaml-variants.4.00.1+open-types";
    "ocaml-variants.4.00.1+PIC";
    "ocaml-variants.4.00.1+raspberrypi";
    "ocaml-variants.4.00.1+short-types";
    "ocaml-variants.4.01.0+32bit";
    "ocaml-variants.4.01.0+armv6-freebsd";
    "ocaml-variants.4.01.0+BER";
    "ocaml-variants.4.01.0+fp";
    "ocaml-variants.4.01.0+musl";
    "ocaml-variants.4.01.0+musl+static";
    "ocaml-variants.4.01.0+open-types";
    "ocaml-variants.4.01.0+PIC";
    "ocaml-variants.4.01.0+profile";
    "ocaml-variants.4.02.1+musl+static";
    "ocaml-variants.4.02.3+musl+static";
    "ocaml-variants.4.04.0+trunk+forced_lto";
    "ocaml-variants.4.06.0+force-safe-string";
    "ocaml-variants.4.06.1+force-safe-string";
    "ocaml-variants.4.06.1+termux";
    "ocaml-variants.4.07.0+force-safe-string";
    "ocaml-variants.4.07.1+force-safe-string";
    "ocaml-variants.4.08.0+force-safe-string";
    "ocaml-variants.4.08.1+force-safe-string";
    "ocaml-variants.4.09.0+32bit";
    "ocaml-variants.4.09.0+afl";
    "ocaml-variants.4.09.0+bytecode-only";
    "ocaml-variants.4.09.0+default-unsafe-string";
    "ocaml-variants.4.09.0+flambda";
    "ocaml-variants.4.09.0+flambda+no-flat-float-array";
    "ocaml-variants.4.09.0+force-safe-string";
    "ocaml-variants.4.09.0+fp";
    "ocaml-variants.4.09.0+fp+flambda";
    "ocaml-variants.4.09.0+musl+flambda";
    "ocaml-variants.4.09.0+musl+static+flambda";
    "ocaml-variants.4.09.0+no-flat-float-array";
    "ocaml-variants.4.09.0+spacetime";
  ] in
  let build_test name version additional_packages =
    let stage_name_suffix =
      let mangle name =
        let name =
          if String.starts_with ~prefix:"ocaml-option-" name then
            String.sub name 13 (String.length name - 13)
          else
            name
        in
        String.map (function '+' -> '-' | c -> c) (String.lowercase_ascii name)
      in
      let suffix =
        if additional_packages = [] then
          ""
        else
          "-" ^ String.concat "-" (List.map mangle additional_packages)
      in
      mangle name ^ suffix
    in
    let stage_name (distro : Dockerfile_opam.Distro.distro) =
      let distro_name =
        Dockerfile_opam.Distro.human_readable_short_string_of_distro
          (distro :> Dockerfile_opam.Distro.t)
      in
      String.lowercase_ascii distro_name ^ "-" ^ stage_name_suffix
    in
    let is_musl_package = is_musl_package name additional_packages in
    let repo, additional_packages =
      if is_musl_package then
        (* XXX No PR proposed as yet! *)
        Some "dra27=git+https://github.com/dra27/opam-repository.git#musl-depexts", additional_packages
      else match name with
      | "ocaml-variants.5.4.1+trunk" ->
          (* Fixed in ocaml/opam-repository#28793 *)
          Some "dra27=git+https://github.com/dra27/opam-repository.git#5.4-trunk", additional_packages
      | "ocaml-variants.5.0.0+tsan" ->
          (* Fixed in ocaml/opam-repository#28794 *)
          None, "conf-unwind" :: additional_packages
      | "ocaml-variants.4.12.0+domains"
      | "ocaml-variants.4.12.0+domains+effects" ->
          (* XXX No PR proposed as yet! *)
          Some "dra27=git+https://github.com/dra27/opam-repository.git#alpine-multicore", additional_packages
      | "ocaml-variants.4.04.0+trunk+forced_lto" ->
          (* XXX No PR proposed as yet! *)
          Some "dra27=git+https://github.com/dra27/opam-repository.git#fix-lto", additional_packages
      | _ ->
          None, additional_packages
    in
    let gcc10_reason = Some "Requires GCC < 10.0.0 (to avoid -fno-common default)" in
    (* XXX Description needed! implicit declarations? *)
    let gcc14_reason = Some "Requires GCC < 14.0.0 (to avoid ???)" in
    (* XXX Description needed! default language change? *)
    let gcc15_reason = Some "Requires GCC < 15.0.0 (to avoid ???)" in
    if requires_32bit name additional_packages then
      let version, explanation =
        if StringSet.mem name gcc10 then
          `V10, gcc10_reason
        else
          `V12, Some "Last i386 version of Debian"
      in
      let distro = `Debian version in
      [Either.Right (stage_name distro, (*`I386*)`X86_64, distro, explanation, repo, (name :: additional_packages))]
    else
      let gcc_constraint =
        if StringSet.mem name gcc10 then
          `GCC10
        else if Ocaml_version.compare version Releases.v4_08_0 < 0 then
          `GCC14
        else if Ocaml_version.compare version Releases.v4_14_2 < 0
                || Ocaml_version.major version = 5
                   && Ocaml_version.minor version = 0 then
          `GCC15
        else
          `None
      in
      let f (latest_distro : Dockerfile_opam.Distro.distro) =
        let distro, explanation =
          match latest_distro, gcc_constraint with
          | `Ubuntu _, `GCC10 ->
              (* Ubuntu 20.10 uses gcc 10.2.0 *)
              Some (`Ubuntu `V20_04), gcc10_reason
          | `Ubuntu _, `GCC14 ->
              (* Ubuntu 24.10 uses gcc 14.2.0 *)
              Some (`Ubuntu `V24_04), gcc14_reason
          | `Ubuntu _, `GCC15 ->
              (* Ubuntu 25.10 uses gcc 15.2.0 *)
              Some (`Ubuntu `V25_04), gcc15_reason
          | `Alpine _, `GCC10 ->
              Some (`Alpine `V3_12), gcc10_reason
          | `Alpine _, `GCC14 ->
              Some (`Alpine `V3_20), gcc14_reason
          | `Alpine _, `GCC15 ->
              Some (`Alpine `V3_20), gcc15_reason
          | `Archlinux `Latest, `GCC10 ->
              None, gcc10_reason
          | `Archlinux `Latest, `GCC14 ->
              None, gcc14_reason
          | `Archlinux `Latest, `GCC15 ->
              None, gcc15_reason
          | `CentOS _, `GCC10 ->
              Some (`CentOS `V8), gcc10_reason
          | `CentOS _, `GCC14 ->
              Some (`CentOS `V9), gcc14_reason
          | `CentOS _, `GCC15 ->
              Some (`CentOS `V9), gcc15_reason
          | `Debian _, `GCC10 ->
              Some (`Debian `V10), gcc10_reason
          | `Debian _, `GCC14 ->
              Some (`Debian `V12), gcc14_reason
          | `Debian _, `GCC15 ->
              Some (`Debian `V12), gcc15_reason
          | `Fedora _, `GCC10 ->
              None, gcc10_reason
          | `Fedora _, `GCC14 ->
              Some (`Fedora `V39), gcc14_reason
          | `Fedora _, `GCC15 ->
              Some (`Fedora `V41), gcc15_reason
          | `OpenSUSE _, `GCC10 ->
              Some (`OpenSUSE `V15_6), gcc10_reason
          | `OpenSUSE _, `GCC14 ->
              Some (`OpenSUSE `V15_6), gcc14_reason
          | `OpenSUSE _, `GCC15 ->
              Some (`OpenSUSE `V15_6), gcc15_reason
          | `OracleLinux _, `GCC10 ->
              Some (`OracleLinux `V8), gcc10_reason
          | `OracleLinux _, `GCC14 ->
              Some (`OracleLinux `V9), gcc14_reason
          | `OracleLinux _, `GCC15 ->
              Some (`OracleLinux `V9), gcc15_reason
          | distro, _ ->
              Some distro, None
        in
        let distro, explanation =
          let is_tsan =
            List.mem "ocaml-option-tsan" additional_packages
            || String.ends_with ~suffix:"+tsan" name
          in
          match distro with
          | Some ((`Alpine _ | `CentOS _ | `Fedora _ | `OracleLinux _))
            when is_tsan ->
              (* XXX To be investigated *)
              None, Some "libtsan required?"
          | Some `Archlinux _
            when is_tsan (*List.mem (Ocaml_version.major version, Ocaml_version.minor version, Option.get (Ocaml_version.patch version))
                 [(5, 2, 0); (5, 2, 1); (5, 3, 0); (5, 4, 0)]*) ->
              (* To be investigated... *)
              None, Some "TSAN alarm failure"
          | _ ->
              distro, explanation
        in
        let distro, explanation =
          match name, distro with
          | ("ocaml-variants.4.12.0+domains" | "ocaml-variants.4.12.0+domains+effects"),
            Some (`Alpine _) ->
              (* libexecinfo-dev required (removed in 3.17+) *)
              Some (`Alpine `V3_16), Some "libexecinfo-dev required, which was removed after Alpine 3.16)"
          | "ocaml-variants.4.04.0+trunk+forced_lto",
            Some (`OpenSUSE _) ->
              (* XXX No sigaltstack patch - need glibc < 2.34 *)
              Some (`OpenSUSE `V15_5), Some "Need glibc < 2.34 (sigaltstack patch missing)"
          | "ocaml-base-compiler.3.08.3",
            Some (`Alpine _) ->
              (* XXX Temporary - needs a patch from 3.08.4 to .depend in graph *)
              None, Some "Patch required to otherlibs/graph/.depend"
          | ("ocaml-base-compiler.3.11.0" | "ocaml-base-compiler.3.11.1"),
            Some (`Alpine _) ->
              (* XXX Segfaulting/unstable on Alpine? (3.11.2 seems fine) *)
              None, Some "Unexpectedly unstable on Alpine"
          | _ ->
              distro, explanation
        in
        let distro, explanation =
          match distro with
          | Some (`OracleLinux _ | `CentOS _ | `OpenSUSE _)
            when is_musl_package ->
              (* No musl support *)
              None, Some "Missing depext support for musl"
          | _ ->
              distro, explanation
        in
        let distro, explanation =
          if StringSet.mem name musl_clang then
            (* XXX These should all be being attempted and tagged as not having depexts? *)
            match distro with
            | Some (`Debian _) ->
                (* XXX Not yet figured out the clang ones for Debian *)
                None, Some "Unclear how to get musl-clang wrapper on Debian"
            | Some (`Ubuntu _) ->
                (* XXX Not yet figured out the clang ones for Ubuntu *)
                None, Some "Unclear how to get musl-clang wrapper on Ubuntu"
            | Some (`Archlinux _) ->
                distro, explanation
            | Some (`Alpine _) ->
                (* This is very dirty because Alpine 3.20 has clang 17 but we
                   need 3.17 with clang 15 for these old versions (implicit
                   declarations). We then have to go back to 3.12 to get clang
                   10 in order not to hit ocmal/ocaml#9981 *)
                Some (`Alpine `V3_12), Some "clang < 11 needed in order to avoid ocaml/ocaml#9981"
            | Some (`Fedora _)
            | None ->
                distro, explanation
            | Some _ ->
                assert false
          else
            distro, explanation
        in
        match distro, explanation with
        | None, Some explanation ->
            Either.Left (latest_distro, explanation)
        | None, None ->
            assert false
        | Some distro, explanation ->
            Either.Right (stage_name distro, `X86_64, distro, explanation, repo, name :: additional_packages)
      in
      List.map f distros
  in
  let expand_package (name, _) =
    let version =
      let i = String.index name '.' + 1 in
      String.sub name i (String.length name - i)
      |> Ocaml_version.of_string_exn 
    in
    match StringMap.find_opt name excludes with
    | Some reason ->
        [(name, version, [], Either.Left reason)]
    | None ->
        let version =
          if Ocaml_version.major version = 3
             && Ocaml_version.minor version = 7 then
            let patch =
              Option.map int_of_string (Ocaml_version.extra version)
            in
            Ocaml_version.with_patch
              (Ocaml_version.without_variant version)
              (if patch = None then Some 0 else patch)
          else
            version
        in
        (* XXX This could be done for the appropriate trunk packages, or with a
               wider matrix (afl+flambda, et. al) for the latest release,
               etc. *)
        let options =
          let all = Fun.const true
          and not_5_0_0 version =
            Ocaml_version.major version <> 5
            || Ocaml_version.minor version <> 0
            || Ocaml_version.patch version <> Some 0
          and since_5_1_0 version =
            Ocaml_version.major version = 5 && Ocaml_version.minor version >= 1
          and since_5_2_0 version =
            Ocaml_version.major version = 5 && Ocaml_version.minor version >= 2
          in [
          "ocaml-option-32bit", all;
          "ocaml-option-afl", all;
          "ocaml-option-bytecode-only", all;
          "ocaml-option-flambda", all;
          "ocaml-option-fp", not_5_0_0;
          "ocaml-option-musl", all;
          "ocaml-option-no-compression", since_5_1_0;
          "ocaml-option-no-flat-float-array", all;
          "ocaml-option-static", all;
          "ocaml-option-tsan", since_5_2_0;
        ] in
        let extras =
          if String.ends_with ~suffix:"+options" name then
            let f (option_package, filter) =
              if filter version then
                Some ([option_package])
              else
                None
            in
            List.filter_map f options
          else
            []
        in
        List.map
          (fun extras -> name, version, extras, Either.Right (build_test name version extras))
          ([] :: extras)
(*
        (name, Either.Right (List.flatten (List.map build_test ((name, version, [])::extras))))
*)
  in
  let tests = List.flatten (List.map expand_package packages) in
  let builds =
    let f (_, _, _, action) =
      match action with
      | Either.Left _ ->
          []
      | Either.Right builds ->
          List.filter_map Either.find_right builds
    in
    List.flatten (List.map f tests)
  in
  (* TODO report on packages - i.e. determine what's built and not built, etc. *)
(*
  create_cache_stage opam_repository_sha opam_repository_archive_sha packages excludes builds;
*)
  let splits =
    let rec split_up l =
      match l with
      | [] ->
          []
      | [_; _] | [_] ->
          [l]
      | d1 :: d2 :: ([_; _] as l) ->
          [[d1; d2]; l]
      | d1 :: d2 :: d3 :: l ->
          [d1; d2; d3] :: split_up l
    in
    split_up distros
  in
  let process_distros index (distros : Dockerfile_opam.Distro.distro list) =
(*
    let short_name =
      String.lowercase_ascii (Dockerfile_opam.Distro.human_readable_short_string_of_distro (distro :> Dockerfile_opam.Distro.t))
    in
*)
    let name = Printf.sprintf "batch-%d" index in
    let filter (d : Dockerfile_opam.Distro.distro) =
      List.exists (Dockerfile_opam.Distro.is_same_distro (d :> Dockerfile_opam.Distro.t)) (distros :> Dockerfile_opam.Distro.t list)
    in
    emit_stage filter name opam_repository_sha opam_repository_archive_sha borked excludes packages builds;
    succ index
  in 
  let _ = List.fold_left process_distros 0 splits in
  emit_stage (Fun.const true) "main" opam_repository_sha opam_repository_archive_sha borked excludes packages builds;
  let report oc (package, _, additional_packages, action) =
    let notes =
      let additional_packages =
        List.filter (String.starts_with ~prefix:"ocaml-option-") additional_packages
      in
      if additional_packages = [] then
        ""
      else
        " with " ^ String.concat ", " additional_packages
    in
    let print fmt = Printf.fprintf oc fmt in
    print "- %s%s" package notes;
    match action with
    | Either.Left explanation ->
        print "; skipped (%s)\n" explanation
    | Either.Right builds ->
        let print_build = function
        | Either.Left (distro, explanation) ->
            let distro =
              Dockerfile_opam.Distro.human_readable_short_string_of_distro (distro :> Dockerfile_opam.Distro.t)
            in
            print "\n  - %s; skipped (%s)" distro explanation
        | Either.Right (_, _, _, None, _, _) ->
            ()
        | Either.Right (_, arch, (distro : Dockerfile_opam.Distro.distro), Some explanation, repo, _) ->
            let arch =
              if arch = `X86_64 then
                ""
              else
                " on " ^ Ocaml_version.string_of_arch arch
            in
            let distro =
              Dockerfile_opam.Distro.human_readable_string_of_distro (distro :> Dockerfile_opam.Distro.t)
            in
            print "\n  - %s%s (%s)" distro arch explanation;
            Option.iter (print "\n    - Using %s") repo
        in
        let compare_build l r =
          match l, r with
          | Either.Left _, Either.Right _ ->
              1
          | Either.Left ((l : Dockerfile_opam.Distro.distro), _), Either.Left ((r : Dockerfile_opam.Distro.distro), _) ->
              Dockerfile_opam.Distro.compare (l :> Dockerfile_opam.Distro.t) (r :> Dockerfile_opam.Distro.t)
          | Either.Right _, Either.Left _ ->
              -1
          | Either.Right (l_name, l_arch, (l_distro : Dockerfile_opam.Distro.distro), _, _, _),
            Either.Right (r_name, r_arch, (r_distro : Dockerfile_opam.Distro.distro), _, _, _) ->
              let result = String.compare l_name r_name in
              let result =
                if result = 0 then
                  compare l_arch r_arch
                else
                  result
              in
              if result = 0 then
                Dockerfile_opam.Distro.compare (l_distro :> Dockerfile_opam.Distro.t) (r_distro :> Dockerfile_opam.Distro.t)
              else
                result
        in
        List.iter print_build (List.sort compare_build builds);
        print "\n"
  in
  let compare_test (l, _, l_add, _) (r, _, r_add, _) =
    let result = String.compare l r in
    if result = 0 then
      compare l_add r_add
    else
      result
  in
  Printf.printf "Creating Status.md\n%!";
  let emit_status oc = List.iter (report oc) (List.sort compare_test tests) in
  Out_channel.with_open_text "Status.md" emit_status;
  let filter_tests = function
  | (name, version, additional_packages, Either.Right tests) ->
      let tests = List.filter_map Either.find_right tests in
      if tests = [] then
        None
      else
        Some (List.map (fun action -> name, version, additional_packages, action) tests)
  | (_, _, _, Either.Left _) ->
      None
  in
  List.flatten (List.filter_map filter_tests tests)

(* Packages which shouldn't be built on any system *)
let excludes =
  let old_trunk = "Old, now unavailable, +trunk package" in
  StringMap.of_list [
  (* XXX Not available - can we test the formula with a partial evaluate for this? *)
  "ocaml-variants.4.04.0+copatterns", old_trunk;
  "ocaml-variants.4.14.1+trunk", old_trunk;
  "ocaml-variants.4.14.2+trunk", old_trunk;
  "ocaml-variants.5.0.0+trunk", old_trunk;
  "ocaml-variants.5.1.0+trunk", old_trunk;
  "ocaml-variants.5.1.1+trunk", old_trunk;
  "ocaml-variants.5.2.0+trunk", old_trunk;
  "ocaml-variants.5.2.1+trunk", old_trunk;
  (* XXX Not solvable on Linux - also testable via availability? *)
  "ocaml-variants.5.2.0+msvc", "Windows-only package";
  (* XXX Isn't really buildable any more, and probably ought to be disabled *)
  "ocaml-variants.4.01.0+lsb", "Unbuildable in 2025";
]

(*
let run_with_result file =
  let cmd = Bos.Cmd.(v "docker" % "build" % "-f" % file % ".") in

  let result = Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.out_lines in

  match result with
  | Ok (lines, (status_info, exit_status)) ->
      let exit_code = match exit_status with
        | `Exited n -> n
        | `Signaled n -> 128 + n
      in
      (lines, exit_code)
  | Error _ -> ([], 1) (* Don't care... for now... check what this actually means! *)
*)

let stack_em_pack_em_and_rack_em opam_repository_sha opam_repository_archive_sha packages excludes builds =
  let cores = ref 64 in
  let cores_lock = Eio.Mutex.create () in
  let freed = Eio.Condition.create () in
  let execute env sw (_nv, version, _additional_packages, ((stage, _arch, _distro, _explanation, _repo, _pacakges) as build)) =
    let dockerfile =
      let dockerfile = "Dockerfile.build-" ^ stage in
      Out_channel.with_open_text dockerfile @@ fun oc ->
        let bases = create_cache_stage oc opam_repository_sha opam_repository_archive_sha packages excludes [build] in
        let _ = List.fold_left (build_package oc ~slow:false ~pin:false (Fun.const true) opam_repository_sha opam_repository_archive_sha StringMap.empty) (None, bases) [build]
        in
        dockerfile
    in
    let required =
      if (Ocaml_version.major version, Ocaml_version.minor version) >= (4, 7) then
        16
      else
        1
    in
    let acquire required =
      let rec loop () =
        if !cores >= required then begin
          cores := !cores - required;
          Eio.Mutex.unlock cores_lock
        end else
          loop ((*Printf.printf "%s: waiting for %d cores" dockerfile required;*) Eio.Condition.await freed cores_lock)
      in
      loop (Eio.Mutex.lock cores_lock)
    in
    let () = acquire required in
    let mgr = Eio.Stdenv.process_mgr env in
    Printf.printf "Starting %s\n%!" dockerfile;
    let stdout_r, stdout = Eio.Process.pipe mgr ~sw in
    let stderr_r, stderr = Eio.Process.pipe mgr ~sw in
    let proc = Eio.Process.spawn ~sw mgr ~stdout ~stderr ["docker"; "build"; "--output"; "type=cacheonly"; "--progress=plain"; "-f"; dockerfile; "."] in
    let read_lines flow =
      let r = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
      let lines = ref [] in
      let rec loop seen_done =
        match Eio.Buf_read.line r with
        | line ->
            let seen_done =
              if String.starts_with ~prefix:"#1 DONE " line then begin
                Printf.printf "Building %s\n%!" dockerfile;
                true
              end else
                seen_done
            in
            lines := line :: !lines;
            loop seen_done
        | exception End_of_file ->
            List.rev !lines
      in
      loop false
    in
    let stdout_lines = ref [] in
    let stderr_lines = ref [] in
    let () =
      Eio.Flow.close stdout;
      Eio.Flow.close stderr;

      Eio.Fiber.both
        (fun () -> stdout_lines := read_lines stdout_r)
        (fun () -> stderr_lines := read_lines stderr_r)
    in
    let status = Eio.Process.await proc in
    let result =
      match status with
      | `Exited n -> n
      | `Signaled n -> 128 + n
    in
    Eio.Mutex.use_rw ~protect:false cores_lock @@ fun () ->
      cores := !cores + required;
      Eio.Condition.broadcast freed;
      Printf.printf "Completed %s with exit code %d\n" dockerfile result;
      result
  in
  Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
      Eio.Fiber.List.map (execute env sw) builds

let main () =
  (* Analyse the Git repositories and return the complete list of analysed opam
     files *)
  let+ all_packages = get_packages () in
  (* Remove ocaml-compiler - it's tested indirectly via other packages *)
  let packages =
    let p =
      List.filter (fun (s, _) -> not (String.starts_with ~prefix:"ocaml-compiler." s))
    in
    p all_packages
  in
  (* XXX These two shas should be plumbed from elsewhere - i.e. from the HEAD
         commits analysed previously by get_packages *)
  let opam_repository_sha = "c603e596bfaf195a5c1a3baf992b11c1de5ad35b" in
  let opam_repository_archive_sha = "7098d3c8e5dda3ac4c6b1f52e189b9b9f0d8c8e6" in
  let tests = generate_test_files opam_repository_sha opam_repository_archive_sha excludes packages in
  let _ =
    Printf.printf "Creating Dockerfile.basis\n%!";
    Out_channel.with_open_text "Dockerfile.basis" @@ fun oc ->
      let builds = List.map (function (_, _, _, build) -> build) tests in
      let bases = create_cache_stage oc opam_repository_sha opam_repository_archive_sha packages excludes builds in
      Printf.fprintf oc {|
FROM ocaml/opam:ubuntu-25.10-opam AS collect
RUN --mount=from=cache,src=/home/opam,dst=/mnt/opam-cache \
|};
      let _ =
        StringSet.iter (fun base ->
          Printf.fprintf oc "    --mount=from=%s,src=/home/opam,dst=/mnt/opam-%s \\\n" base base)
            bases
      in
      Printf.fprintf oc {|    cat /mnt/opam-*/epoch > /home/opam/results
|}
  in
  let () =
    Printf.printf "Executing Dockerfile.basis\n%!";
    (* XXX bos-ify etc... although there is a tiny benefit that this gives the console!! *)
    if Sys.command "docker build -f Dockerfile.basis ." <> 0 then begin
      prerr_endline "Preparing basis failed?!"; raise Exit
    end
  in
  let _ = stack_em_pack_em_and_rack_em opam_repository_sha opam_repository_archive_sha packages excludes tests in
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
      | e ->
          Lwt_io.eprintf "Unhandled exception: %s\n" (Printexc.to_string e))
  end
