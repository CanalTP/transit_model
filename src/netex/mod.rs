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

//! [Netex](http://netex-cen.eu/) format management.

mod read;

use model::{Collections, Model};
use std::fs;
use std::path::Path;
use utils::{add_prefix_to_collection, add_prefix_to_collection_with_id};
use Result;
extern crate tempdir;
extern crate zip;

fn add_prefix(prefix: String, collections: &mut Collections) -> Result<()> {
    let prefix = prefix + ":";
    info!("Adding prefix: \"{}\"", &prefix);
    add_prefix_to_collection_with_id(&mut collections.commercial_modes, &prefix)?;
    add_prefix_to_collection_with_id(&mut collections.networks, &prefix)?;
    add_prefix_to_collection_with_id(&mut collections.companies, &prefix)?;
    add_prefix_to_collection_with_id(&mut collections.stop_points, &prefix)?;
    add_prefix_to_collection_with_id(&mut collections.stop_areas, &prefix)?;
    add_prefix_to_collection(&mut collections.transfers, &prefix);
    add_prefix_to_collection_with_id(&mut collections.routes, &prefix)?;
    add_prefix_to_collection_with_id(&mut collections.lines, &prefix)?;
    add_prefix_to_collection_with_id(&mut collections.contributors, &prefix)?;
    add_prefix_to_collection_with_id(&mut collections.datasets, &prefix)?;
    add_prefix_to_collection_with_id(&mut collections.vehicle_journeys, &prefix)?;
    add_prefix_to_collection_with_id(&mut collections.trip_properties, &prefix)?;
    add_prefix_to_collection_with_id(&mut collections.equipments, &prefix)?;
    add_prefix_to_collection_with_id(&mut collections.comments, &prefix)?;

    Ok(())
}

/// Imports a `Model` from one or several [Netex](http://netex-cen.eu/) files.
/// The `path` can be a single file, a directory or a zip file.
/// Refers to the [Netex Github repo](https://github.com/NeTEx-CEN/NeTEx/)
/// for details.
///
/// The `config_path` argument allows you to give a path to a file
/// containing a json representing the contributor and dataset used
/// for this Netex file. If not given, default values will be created.
///
/// The `prefix` argument is a string that will be prepended to every
/// identifiers, allowing to namespace the dataset. By default, no
/// prefix will be added to the identifiers.
pub fn read<P>(path: P, config_path: Option<P>, prefix: Option<String>) -> Result<Model>
where
    P: AsRef<Path>,
{
    let path = path.as_ref();
    info!("Reading Netex data from {:?}", path);
    println!("Reading Netex data from {:?}", path);
    let mut collections = Collections::default();
    if path.is_file() {
        match path.extension().unwrap().to_str().unwrap() {
            "zip" => {
                // let input_tmp_dir =
                //     Path::new("fixtures/netex/RATP_Line7bis-extract-2009-NeTEx/input_tmp");
                // ::utils::unzip_to(path.as_ref(), input_tmp_dir);
                let zip_file = fs::File::open(path)?;
                let mut zip = zip::ZipArchive::new(zip_file)?;
                for i in 0..zip.len() {
                    let mut file = zip.by_index(i)?;
                    match file.sanitized_name().extension() {
                        None => info!("Netex read : skipping file in ZIP : {:?}", file.sanitized_name()),
                        Some(ext) => {
                            if ext == "xml" {
                                read::read_netex_file(&mut collections, file)?;
                            } else {
                                info!("Netex read : skipping file in ZIP : {:?}", file.sanitized_name()),
                            }
                        },
                    }
                }
            }
            "xml" => read::read_netex_file(&mut collections, fs::File::open(path)?)?,
            _ => bail!("Provided netex file should be xml or zip : {:?}", path),
        };
    } else {
        for entry in fs::read_dir(path)? {
            let file = entry?;
            if file.path().extension().unwrap() == "xml" {
                let file = fs::File::open(file.path())?;
                read::read_netex_file(&mut collections, file)?;
            }
        }
    };

    let (contributors, datasets) = read::read_config(config_path)?;
    collections.contributors = contributors;
    collections.datasets = datasets;

    //add prefixes
    if let Some(prefix) = prefix {
        add_prefix(prefix, &mut collections)?;
    }

    Ok(Model::new(collections)?)
}
