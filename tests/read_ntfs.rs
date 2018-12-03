// Copyright 2017-2018 Kisio Digital and/or its affiliates.
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

extern crate navitia_model;
use navitia_model::collection::{CollectionWithId, Id, Idx};
use navitia_model::model::{GetCorresponding, Model};
use navitia_model::objects::*;
use navitia_model::relations::IdxSet;
use navitia_model::test_utils::*;

fn get<T, U>(idx: Idx<T>, collection: &CollectionWithId<U>, objects: &Model) -> Vec<String>
where
    U: Id<U>,
    IdxSet<T>: GetCorresponding<U>,
{
    objects
        .get_corresponding_from_idx(idx)
        .iter()
        .map(|idx| collection[*idx].id().to_string())
        .collect()
}

#[test]
fn minimal() {
    let ntm = navitia_model::ntfs::read("fixtures/minimal_ntfs/").unwrap();

    assert_eq!(8, ntm.stop_areas.len());
    assert_eq!(12, ntm.stop_points.len());
    assert_eq!(3, ntm.commercial_modes.len());
    assert_eq!(3, ntm.lines.len());
    assert_eq!(6, ntm.routes.len());
    assert_eq!(3, ntm.physical_modes.len());
    assert_eq!(6, ntm.vehicle_journeys.len());
    assert_eq!(1, ntm.networks.len());
    assert_eq!(1, ntm.companies.len());
    assert_eq!(1, ntm.contributors.len());
    assert_eq!(1, ntm.datasets.len());
    assert_eq!(0, ntm.geometries.len());

    let gdl = ntm.stop_areas.get_idx("GDL").unwrap();
    assert_eq!(3, ntm.get_corresponding_from_idx::<_, StopPoint>(gdl).len());
    assert_eq!(
        get(gdl, &ntm.physical_modes, &ntm),
        &["Bus", "Metro", "RapidTransit"]
    );
    assert_eq!(
        get(gdl, &ntm.commercial_modes, &ntm),
        &["Bus", "Metro", "RER"]
    );
    assert_eq!(get(gdl, &ntm.networks, &ntm), &["TGN"]);
    assert_eq!(get(gdl, &ntm.contributors, &ntm), &["TGC"]);

    let rera = ntm.lines.get_idx("RERA").unwrap();
    assert_eq!(
        get(rera, &ntm.physical_modes, &ntm),
        &["Bus", "RapidTransit"]
    );
    assert_eq!(get(rera, &ntm.commercial_modes, &ntm), &["RER"]);
    assert_eq!(get(rera, &ntm.networks, &ntm), &["TGN"]);
    assert_eq!(get(rera, &ntm.contributors, &ntm), &["TGC"]);
    assert_eq!(get(rera, &ntm.routes, &ntm), &["RERAF", "RERAB"]);
    assert_eq!(
        get(rera, &ntm.vehicle_journeys, &ntm),
        &["RERAF1", "RERAB1"]
    );
    assert_eq!(
        get(rera, &ntm.stop_points, &ntm),
        &["GDLR", "NATR", "CDGR", "DEFR"]
    );
    assert_eq!(
        get(rera, &ntm.stop_areas, &ntm),
        &["GDL", "NAT", "CDG", "DEF"]
    );
}

#[test]
fn ntfs_stop_zones() {
    let ntm = navitia_model::ntfs::read("fixtures/minimal_ntfs/").unwrap();
    let stop_zone_1 = ntm.stop_points.get("MTPZ").unwrap();
    assert_eq!(stop_zone_1.stop_type, StopType::Zone);
    let stop_zone_2 = ntm.stop_points.get("CDGZ").unwrap();
    assert_eq!(stop_zone_2.stop_type, StopType::Zone);
}

#[test]
fn ntfs_stops_output() {
    let ntm = navitia_model::ntfs::read("fixtures/minimal_ntfs/").unwrap();
    test_in_tmp_dir(|output_dir| {
        navitia_model::ntfs::write(&ntm, output_dir).unwrap();
        compare_output_dir_with_expected(&output_dir, vec!["stops.txt"], "fixtures/ntfs2ntfs");
    });
}

#[test]
fn ntfs() {
    let pt_objects = navitia_model::ntfs::read("fixtures/ntfs/").unwrap();

    // comments
    use CommentType::*;
    fn assert_eq_comment(comment: &Comment, id: &str, name: &str, comment_type: CommentType) {
        let expect = Comment {
            id: id.to_string(),
            name: name.to_string(),
            comment_type,
            label: None,
            url: None,
        };
        assert_eq!(comment, &expect);
    }
    assert_eq!(4, pt_objects.comments.len());
    let rera_lines_idx = pt_objects.lines.get_idx("RERA").unwrap();
    let rera_comment_indexes = &pt_objects.lines[rera_lines_idx].comment_links;
    let mut iter = pt_objects.comments.iter_from(rera_comment_indexes);
    assert_eq_comment(
        iter.next().unwrap(),
        "RERACOM1",
        "some information",
        Information,
    );
    assert_eq_comment(
        iter.next().unwrap(),
        "RERACOM2",
        "strange comment type",
        Information,
    );
    assert_eq_comment(
        iter.next().unwrap(),
        "RERACOM3",
        "no comment type",
        Information,
    );
    assert_eq_comment(
        iter.next().unwrap(),
        "RERACOM4",
        "on demand transport comment",
        OnDemandTransport,
    );
    assert_eq!(iter.next(), None);
}

#[test]
fn optional_empty_collections_not_created() {
    let ntm = navitia_model::ntfs::read("fixtures/minimal_ntfs/").unwrap();
    test_in_tmp_dir(|path| {
        navitia_model::ntfs::write(&ntm, path).unwrap();

        use std::collections::HashSet;
        let entries: HashSet<String> = ::std::fs::read_dir(path)
            .unwrap()
            .map(|e| e.unwrap().file_name().into_string().unwrap())
            .collect();
        assert!(!entries.contains("comments.txt"));
        assert!(!entries.contains("comment_links.txt"));
        assert!(!entries.contains("equipments.txt"));
        assert!(!entries.contains("transfers.txt"));
        assert!(!entries.contains("trip_properties.txt"));
        assert!(!entries.contains("geometries.txt"));
        assert!(!entries.contains("object_properties.txt"));
        assert!(!entries.contains("object_codes.txt"));
        assert!(!entries.contains("admin_stations.txt"));
    });
}
