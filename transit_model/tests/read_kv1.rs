// Copyright 2017 Kisio Digital and/or its affiliates.
//
// This program is free software: you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see
// <http://www.gnu.org/licenses/>.

#[cfg(feature = "proj")]
use transit_model;
#[cfg(feature = "proj")]
use transit_model::test_utils::*;

#[cfg(feature = "proj")]
#[test]
fn test_read_kv1() {
    let ntm = transit_model::kv1::read_from_path(
        "fixtures/kv12ntfs/input",
        Some("fixtures/kv12ntfs/config.json"),
        Some("prefix".into()),
    )
    .unwrap();
    test_in_tmp_dir(|output_dir| {
        transit_model::ntfs::write(&ntm, output_dir, get_test_datetime()).unwrap();
        compare_output_dir_with_expected(&output_dir, None, "fixtures/kv12ntfs/output");
    });
}
