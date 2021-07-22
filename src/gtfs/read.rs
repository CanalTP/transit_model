// Copyright (C) 2017 Kisio Digital and/or its affiliates.
//
// This program is free software: you can redistribute it and/or modify it
// under the terms of the GNU Affero General Public License as published by the
// Free Software Foundation, version 3.

// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
// details.

// You should have received a copy of the GNU Affero General Public License
// along with this program. If not, see <https://www.gnu.org/licenses/>

use super::{
    Agency, DirectionType, Route, RouteType, Shape, Stop, StopLocationType, StopTime, Transfer,
    TransferType, Trip,
};
use crate::{
    model::Collections,
    objects::{
        self, Availability, CommentLinksT, Coord, KeysValues, Pathway, PropertiesMap, StopLocation,
        StopPoint, StopTime as NtfsStopTime, StopTimePrecision, StopType, Time, TransportType,
        VehicleJourney,
    },
    read_utils::{read_collection, read_objects, read_objects_loose, FileHandler},
    utils::*,
    Result,
};
use derivative::Derivative;
use failure::{bail, format_err, Error};
use geo::{LineString, Point};
use log::{info, warn};
use serde::Deserialize;
use skip_error::{skip_error_and_log, SkipError};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::convert::TryFrom;
use typed_index_collection::{impl_id, Collection, CollectionWithId, Idx};

fn default_agency_id() -> String {
    1.to_string()
}

fn get_agency_id(route: &Route, networks: &CollectionWithId<objects::Network>) -> Result<String> {
    route
        .agency_id
        .clone()
        .ok_or(())
        .or_else(|()| match networks.values().next() {
            Some(n) if networks.len() == 1 => Ok(n.id.clone()),
            Some(_) => bail!("Impossible to get agency id, several networks found"),
            None => bail!("Impossible to get agency id, no network found"),
        })
}

impl From<Agency> for objects::Network {
    fn from(agency: Agency) -> objects::Network {
        let id = agency.id.unwrap_or_else(default_agency_id);
        let mut codes = KeysValues::default();
        codes.insert(("source".to_string(), id.clone()));
        objects::Network {
            id,
            name: agency.name,
            codes,
            timezone: Some(agency.timezone),
            url: Some(agency.url),
            lang: agency.lang,
            phone: agency.phone,
            address: None,
            sort_order: None,
        }
    }
}
impl From<Agency> for objects::Company {
    fn from(agency: Agency) -> objects::Company {
        objects::Company {
            id: agency.id.unwrap_or_else(default_agency_id),
            name: agency.name,
            address: None,
            url: Some(agency.url),
            mail: agency.email,
            phone: agency.phone,
            codes: BTreeSet::new(),
        }
    }
}

impl TryFrom<Stop> for objects::StopArea {
    type Error = Error;
    fn try_from(stop: Stop) -> Result<Self> {
        let mut codes: KeysValues = BTreeSet::new();
        codes.insert(("source".to_string(), stop.id.clone()));
        if let Some(c) = stop.code.as_ref() {
            codes.insert(("gtfs_stop_code".to_string(), c.clone()));
        }
        if stop.name.is_empty() {
            warn!("stop_id: {}: for station stop_name is required", stop.id);
        }
        let coord = Coord::from((stop.lon, stop.lat));
        if coord == Coord::default() {
            warn!("stop_id: {}: for station coordinates are required", stop.id);
        }

        let stop_area = objects::StopArea {
            id: stop.id,
            name: stop.name,
            codes,
            object_properties: PropertiesMap::default(),
            comment_links: objects::CommentLinksT::default(),
            coord,
            timezone: stop.timezone,
            visible: true,
            geometry_id: None,
            level_id: stop.level_id,
            equipment_id: None,
        };
        Ok(stop_area)
    }
}

impl TryFrom<Stop> for objects::StopPoint {
    type Error = Error;
    fn try_from(stop: Stop) -> Result<Self> {
        let mut codes: KeysValues = BTreeSet::new();
        codes.insert(("source".to_string(), stop.id.clone()));
        if let Some(c) = stop.code.as_ref() {
            codes.insert(("gtfs_stop_code".to_string(), c.clone()));
        }
        if stop.name.is_empty() {
            warn!("stop_id: {}: for platform name is required", stop.id);
        };

        let coord = Coord::from((stop.lon, stop.lat));
        if coord == Coord::default() {
            warn!(
                "stop_id: {}: for platform coordinates are required",
                stop.id
            );
        }
        let stop_point = objects::StopPoint {
            id: stop.id,
            name: stop.name,
            code: stop.code,
            codes,
            coord,
            stop_area_id: stop
                .parent_station
                .unwrap_or_else(|| String::from("default_id")),
            timezone: stop.timezone,
            visible: true,
            stop_type: StopType::Point,
            platform_code: stop.platform_code,
            level_id: stop.level_id,
            fare_zone_id: stop.fare_zone_id,
            ..Default::default()
        };
        Ok(stop_point)
    }
}

impl TryFrom<Stop> for objects::StopLocation {
    type Error = Error;
    fn try_from(stop: Stop) -> Result<Self> {
        let coord = Coord::from((stop.lon, stop.lat));

        if stop.location_type == StopLocationType::StopEntrance {
            if coord == Coord::default() {
                bail!(
                    "stop_id: {}: for entrances/exits coordinates is required",
                    stop.id
                );
            }
            if stop.parent_station.is_none() {
                bail!(
                    "stop_id: {}: for entrances/exits parent_station is required",
                    stop.id
                );
            }
            if stop.name.is_empty() {
                bail!(
                    "stop_id: {}: for entrances/exits stop_name is required",
                    stop.id
                );
            }
        }
        if stop.location_type == StopLocationType::GenericNode && stop.parent_station.is_none() {
            bail!(
                "stop_id: {}: for generic node parent_station is required",
                stop.id
            );
        }
        if stop.location_type == StopLocationType::BoardingArea && stop.parent_station.is_none() {
            bail!(
                "stop_id: {}: for boarding area parent_station is required",
                stop.id
            );
        }
        let stop_location = StopLocation {
            id: stop.id,
            name: stop.name,
            code: stop.code,
            comment_links: CommentLinksT::default(),
            visible: false,
            coord,
            parent_id: stop.parent_station,
            timezone: stop.timezone,
            geometry_id: None,
            equipment_id: None,
            level_id: stop.level_id,
            stop_type: stop.location_type.into(),
        };
        Ok(stop_location)
    }
}

impl RouteType {
    fn to_gtfs_value(&self) -> String {
        match *self {
            RouteType::Tramway => "0".to_string(),
            RouteType::Metro => "1".to_string(),
            RouteType::Train => "2".to_string(),
            RouteType::Bus
            | RouteType::UnknownMode
            | RouteType::Coach
            | RouteType::Air
            | RouteType::Taxi => "3".to_string(),
            RouteType::Ferry => "4".to_string(),
            RouteType::CableCar => "5".to_string(),
            RouteType::SuspendedCableCar => "6".to_string(),
            RouteType::Funicular => "7".to_string(),
        }
    }
}

impl ::serde::Serialize for RouteType {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ::serde::Serializer,
    {
        serializer.serialize_str(&self.to_gtfs_value())
    }
}

impl<'de> ::serde::Deserialize<'de> for RouteType {
    fn deserialize<D>(deserializer: D) -> Result<RouteType, D::Error>
    where
        D: ::serde::Deserializer<'de>,
    {
        let i = u16::deserialize(deserializer)?;
        let hundreds = i / 100;
        Ok(match (i, hundreds) {
            (0, _) | (_, 9) => RouteType::Tramway,
            (1, _) | (_, 4) | (_, 5) | (_, 6) => RouteType::Metro,
            (2, _) | (_, 1) | (_, 3) => RouteType::Train,
            (3, _) | (_, 7) | (_, 8) => RouteType::Bus,
            (4, _) | (_, 10) | (_, 12) => RouteType::Ferry,
            (5, _) => RouteType::CableCar,
            (6, _) | (_, 13) => RouteType::SuspendedCableCar,
            (7, _) | (_, 14) => RouteType::Funicular,
            (_, 2) => RouteType::Coach,
            (_, 11) => RouteType::Air,
            (_, 15) => RouteType::Taxi,
            _ => RouteType::UnknownMode,
        })
    }
}

impl_id!(Route);

impl Route {
    fn generate_line_key(
        &self,
        read_as_line: bool,
        idx: Idx<Route>,
    ) -> (Option<String>, String, Option<Idx<Route>>) {
        let name = if !self.short_name.is_empty() {
            self.short_name.clone()
        } else {
            self.long_name.clone()
        };
        let key = if read_as_line { Some(idx) } else { None };
        (self.agency_id.clone(), name, key)
    }

    fn get_id_by_direction(&self, d: DirectionType) -> String {
        let id = self.id.clone();
        match d {
            DirectionType::Forward => id,
            DirectionType::Backward => id + "_R",
        }
    }
}

impl Trip {
    fn to_ntfs_vehicle_journey(
        &self,
        routes: &CollectionWithId<Route>,
        dataset: &objects::Dataset,
        trip_property_id: &Option<String>,
        networks: &CollectionWithId<objects::Network>,
    ) -> Result<objects::VehicleJourney> {
        let route = match routes.get(&self.route_id) {
            Some(route) => route,
            None => bail!("Coudn't find route {} for trip {}", self.route_id, self.id),
        };
        let physical_mode = get_physical_mode(&route.route_type);
        let mut codes = KeysValues::default();
        codes.insert(("source".to_string(), self.id.clone()));

        Ok(objects::VehicleJourney {
            id: self.id.clone(),
            codes,
            object_properties: PropertiesMap::default(),
            comment_links: CommentLinksT::default(),
            route_id: route.get_id_by_direction(self.direction),
            physical_mode_id: physical_mode.id,
            dataset_id: dataset.id.clone(),
            service_id: self.service_id.clone(),
            headsign: self.short_name.clone().or_else(|| self.headsign.clone()),
            short_name: None,
            block_id: self.block_id.clone(),
            company_id: get_agency_id(route, networks)?,
            trip_property_id: trip_property_id.clone(),
            geometry_id: self.shape_id.clone(),
            stop_times: vec![],
            journey_pattern_id: None,
        })
    }
}

pub(in crate::gtfs) fn manage_shapes<H>(
    collections: &mut Collections,
    file_handler: &mut H,
) -> Result<()>
where
    for<'a> &'a mut H: FileHandler,
{
    let file = "shapes.txt";
    let mut shapes = read_objects_loose::<_, Shape>(file_handler, file, false)?;
    shapes.sort_unstable_by_key(|s| s.sequence);
    let mut map: HashMap<String, Vec<Point<f64>>> = HashMap::new();
    for s in &shapes {
        map.entry(s.id.clone())
            .or_insert_with(Vec::new)
            .push((s.lon, s.lat).into())
    }

    collections.geometries = CollectionWithId::new(
        map.iter()
            .filter(|(_, points)| !points.is_empty())
            .map(|(id, points)| {
                let linestring: LineString<f64> = points.to_vec().into();
                objects::Geometry {
                    id: id.to_string(),
                    geometry: linestring.into(),
                }
            })
            .collect(),
    )?;

    Ok(())
}

pub(in crate::gtfs) fn manage_stop_times<H>(
    collections: &mut Collections,
    file_handler: &mut H,
    on_demand_transport: bool,
    on_demand_transport_comment: Option<String>,
) -> Result<()>
where
    for<'a> &'a mut H: FileHandler,
{
    let file_name = "stop_times.txt";
    let mut headsigns = HashMap::new();
    let mut tmp_vjs = BTreeMap::new();
    let stop_times = read_objects::<_, StopTime>(file_handler, file_name, true)?;

    for mut stop_time in stop_times {
        if let Some(vj_idx) = collections.vehicle_journeys.get_idx(&stop_time.trip_id) {
            // consume the stop headsign
            if let Some(headsign) = stop_time.stop_headsign.take() {
                headsigns.insert(
                    (stop_time.trip_id.clone(), stop_time.stop_sequence),
                    headsign,
                );
            }

            tmp_vjs
                .entry(vj_idx)
                .or_insert_with(Vec::new)
                .push(stop_time);
        } else {
            warn!(
                "Problem reading {:?}: trip_id={:?} not found. Skipping this stop_time",
                file_name, stop_time.trip_id
            )
        }
    }
    collections.stop_time_headsigns = headsigns;

    for (vj_idx, mut stop_times) in tmp_vjs {
        stop_times.sort_unstable_by_key(|st| st.stop_sequence);
        stop_times.dedup_by(|st2, st1| {
            let is_same_seq = st2.stop_sequence == st1.stop_sequence;
            if is_same_seq {
                warn!(
                    "remove duplicated stop_sequence '{}' of vehicle '{}'",
                    st2.stop_sequence, st2.trip_id
                );
            }
            is_same_seq
        });
        let st_values = interpolate_undefined_stop_times(
            &collections.vehicle_journeys[vj_idx].id,
            &stop_times,
        )?;

        let company_idx = collections
            .companies
            .get_idx(&collections.vehicle_journeys[vj_idx].company_id);

        for (stop_time, st_values) in stop_times.iter().zip(st_values) {
            if let Some(stop_point_idx) = collections.stop_points.get_idx(&stop_time.stop_id) {
                let precision = match (on_demand_transport, st_values.datetime_estimated) {
                    (_, false) => Some(StopTimePrecision::Exact),
                    (false, true) => Some(StopTimePrecision::Approximate),
                    (true, true) => Some(StopTimePrecision::Estimated),
                };

                if let Some(message) = on_demand_transport_comment.as_ref() {
                    if stop_time.pickup_type == 2 || stop_time.drop_off_type == 2 {
                        if let Some(company_idx) = company_idx {
                            manage_odt_comment_from_stop_time(
                                collections,
                                message,
                                company_idx,
                                vj_idx,
                                stop_time,
                            );
                        }
                    }
                }
                collections
                    .vehicle_journeys
                    .index_mut(vj_idx)
                    .stop_times
                    .push(objects::StopTime {
                        stop_point_idx,
                        sequence: stop_time.stop_sequence,
                        arrival_time: st_values.arrival_time,
                        departure_time: st_values.departure_time,
                        boarding_duration: 0,
                        alighting_duration: 0,
                        pickup_type: stop_time.pickup_type,
                        drop_off_type: stop_time.drop_off_type,
                        datetime_estimated: st_values.datetime_estimated,
                        local_zone_id: stop_time.local_zone_id,
                        precision,
                    });
            } else {
                warn!(
                    "Problem reading {:?}: stop_id={:?} not found. Skipping this stop_time",
                    file_name, stop_time.stop_id
                );
            }
        }
    }
    Ok(())
}

fn ventilate_stop_times(
    undefined_stop_times: &[&StopTime],
    before: &StopTimesValues,
    after: &StopTimesValues,
) -> Vec<StopTimesValues> {
    let duration = after.arrival_time - before.departure_time;
    let step = duration / (undefined_stop_times.len() + 1) as u32;
    let mut res = vec![];
    for idx in 0..undefined_stop_times.len() {
        let num = idx as u32 + 1u32;
        let time = before.departure_time + objects::Time::new(0, 0, num * step.total_seconds());
        res.push(StopTimesValues {
            departure_time: time,
            arrival_time: time,
            datetime_estimated: true,
        });
    }
    res
}

// Temporary struct used by the interpolation process
struct StopTimesValues {
    arrival_time: Time,
    departure_time: Time,
    datetime_estimated: bool,
}

// in the GTFS some stoptime can have undefined departure/arrival (all stop_times but the first and the last)
// when it's the case, we apply a simple distribution of those stops, and we mark them as `estimated`
// cf. https://github.com/CanalTP/navitia_model/blob/master/src/documentation/gtfs_read.md#reading-stop_timestxt
fn interpolate_undefined_stop_times(
    vj_id: &str,
    stop_times: &[StopTime],
) -> Result<Vec<StopTimesValues>> {
    let mut undefined_stops_bulk = Vec::with_capacity(0);
    let mut res = vec![];
    for st in stop_times {
        // if only one in departure/arrival value is defined, we set it to the other value
        let (departure_time, arrival_time) = match (st.departure_time, st.arrival_time) {
            (Some(departure_time), None) => {
                log::debug!("for vj '{}', stop time n° {} has no arrival defined, we set it to its departure value", vj_id, st.stop_sequence);
                (departure_time, departure_time)
            }
            (None, Some(arrival_time)) => {
                log::debug!("for vj '{}', stop time n° {} has no departure defined, we set it to its arrival value", vj_id, st.stop_sequence);
                (arrival_time, arrival_time)
            }
            (Some(departure_time), Some(arrival_time)) => (departure_time, arrival_time),
            (None, None) => {
                undefined_stops_bulk.push(st);
                continue;
            }
        };

        let st_value = StopTimesValues {
            departure_time,
            arrival_time,
            datetime_estimated: !st.timepoint,
        };

        if !undefined_stops_bulk.is_empty() {
            let values = ventilate_stop_times(
                &undefined_stops_bulk,
                res.last().ok_or_else(|| format_err!("the first stop time of the vj '{}' has no departure/arrival, the stop_times.txt file is not valid", vj_id))?,
                &st_value,
            );
            res.extend(values);
            undefined_stops_bulk.clear();
        }
        res.push(st_value);
    }

    if !undefined_stops_bulk.is_empty() {
        Err(format_err!("the last stop time of the vj '{}' has no departure/arrival, the stop_times.txt file is not valid", vj_id))
    } else {
        Ok(res)
    }
}

pub(in crate::gtfs) fn read_agency<H>(
    file_handler: &mut H,
) -> Result<(
    CollectionWithId<objects::Network>,
    CollectionWithId<objects::Company>,
)>
where
    for<'a> &'a mut H: FileHandler,
{
    let filename = "agency.txt";
    let gtfs_agencies = read_objects::<_, Agency>(file_handler, filename, true)?;

    if let Some(referent_agency) = gtfs_agencies.first() {
        for agency in gtfs_agencies.iter().skip(1) {
            if referent_agency.timezone != agency.timezone {
                warn!(
                    "different agency timezone: {} ({}) - {} ({})",
                    referent_agency.timezone,
                    referent_agency.id.clone().unwrap_or_default(),
                    agency.timezone,
                    agency.id.clone().unwrap_or_default(),
                );
                break;
            }
        }
    }

    let networks = gtfs_agencies
        .iter()
        .cloned()
        .map(objects::Network::from)
        .collect();
    let networks = CollectionWithId::new(networks)?;
    let companies = gtfs_agencies
        .into_iter()
        .map(objects::Company::from)
        .collect();
    let companies = CollectionWithId::new(companies)?;
    Ok((networks, companies))
}

fn generate_stop_comment(stop: &Stop) -> Option<objects::Comment> {
    stop.desc.as_ref().map(|desc| objects::Comment {
        id: "stop:".to_string() + &stop.id,
        comment_type: objects::CommentType::Information,
        label: None,
        name: desc.to_string(),
        url: None,
    })
}

fn insert_comment<'c, T: typed_index_collection::Id<T> + objects::CommentLinks>(
    collection: &'c mut CollectionWithId<T>,
    comments: &mut CollectionWithId<objects::Comment>,
    prefix: &str,
    gtfs_route: &Route,
) {
    let opt_comment = gtfs_route.desc.as_ref().map(|desc| objects::Comment {
        id: format!("{}:{}", prefix, gtfs_route.id),
        comment_type: objects::CommentType::Information,
        label: None,
        name: desc.to_string(),
        url: None,
    });

    if let Some(comment) = opt_comment {
        if let Some(mut object) = collection.get_mut(&gtfs_route.id) {
            object.comment_links_mut().insert(comment.id.to_string());
            comments
                .push(comment)
                .expect("Duplicated comment id that shouldn’t be possible");
        }
    }
}

fn manage_odt_comment_from_stop_time(
    collections: &mut Collections,
    on_demand_transport_comment: &str,
    company_idx: Idx<objects::Company>,
    vj_idx: Idx<objects::VehicleJourney>,
    stop_time: &StopTime,
) {
    let comment_id = format!("ODT:{}", collections.companies[company_idx].id);
    if !collections.comments.contains_id(&comment_id) {
        let comment = objects::Comment {
            id: comment_id.clone(),
            comment_type: objects::CommentType::OnDemandTransport,
            label: None,
            name: on_demand_transport_comment
                .replace("{agency_name}", &collections.companies[company_idx].name)
                .replace(
                    "{agency_phone}",
                    &collections.companies[company_idx]
                        .phone
                        .clone()
                        .unwrap_or_default(),
                ),
            url: None,
        };
        // Ok to unwrap since we already tested for existence of the identifier
        collections.comments.push(comment).unwrap();
    }
    collections.stop_time_comments.insert(
        (
            collections.vehicle_journeys[vj_idx].id.to_string(),
            stop_time.stop_sequence,
        ),
        comment_id,
    );
    let stop_time_id = format!("{}-{}", stop_time.trip_id, stop_time.stop_sequence);
    collections.stop_time_ids.insert(
        (
            collections.vehicle_journeys[vj_idx].id.to_string(),
            stop_time.stop_sequence,
        ),
        stop_time_id,
    );
}

#[derive(Default)]
pub struct EquipmentList {
    equipments: HashMap<objects::Equipment, String>,
}

impl EquipmentList {
    pub fn into_equipments(self) -> Vec<objects::Equipment> {
        let mut eqs: Vec<_> = self
            .equipments
            .into_iter()
            .map(|(mut eq, id)| {
                eq.id = id;
                eq
            })
            .collect();

        eqs.sort_by(|l, r| l.id.cmp(&r.id));
        eqs
    }

    pub fn push(&mut self, equipment: objects::Equipment) -> String {
        let equipment_id = self.equipments.len().to_string();
        let id = self.equipments.entry(equipment).or_insert(equipment_id);
        id.clone()
    }
}

fn get_equipment_id_and_populate_equipments(
    equipments: &mut EquipmentList,
    stop: &Stop,
) -> Option<String> {
    match stop.wheelchair_boarding {
        Availability::Available | Availability::NotAvailable => {
            Some(equipments.push(objects::Equipment {
                id: "".to_string(),
                wheelchair_boarding: stop.wheelchair_boarding,
                sheltered: Availability::InformationNotAvailable,
                elevator: Availability::InformationNotAvailable,
                escalator: Availability::InformationNotAvailable,
                bike_accepted: Availability::InformationNotAvailable,
                bike_depot: Availability::InformationNotAvailable,
                visual_announcement: Availability::InformationNotAvailable,
                audible_announcement: Availability::InformationNotAvailable,
                appropriate_escort: Availability::InformationNotAvailable,
                appropriate_signage: Availability::InformationNotAvailable,
            }))
        }
        _ => None,
    }
}

pub(in crate::gtfs) fn read_stops<H>(
    file_handler: &mut H,
    comments: &mut CollectionWithId<objects::Comment>,
    equipments: &mut EquipmentList,
) -> Result<(
    CollectionWithId<objects::StopArea>,
    CollectionWithId<objects::StopPoint>,
    CollectionWithId<objects::StopLocation>,
)>
where
    for<'a> &'a mut H: FileHandler,
{
    info!("Reading stops.txt");
    let file = "stops.txt";
    let gtfs_stops = read_objects::<_, Stop>(file_handler, file, true)?;
    let mut stop_areas = vec![];
    let mut stop_points = vec![];
    let mut stop_locations = vec![];
    for stop in gtfs_stops {
        let mut comment_links = CommentLinksT::default();
        if let Some(comment) = generate_stop_comment(&stop) {
            comment_links.insert(comment.id.to_string());
            comments
                .push(comment)
                .expect("Duplicated comment id that shouldn’t be possible");
        }
        let equipment_id = get_equipment_id_and_populate_equipments(equipments, &stop);
        match stop.location_type {
            StopLocationType::StopPoint => {
                let mut stop_point = skip_error_and_log!(
                    objects::StopPoint::try_from(stop.clone()),
                    tracing::Level::WARN
                );
                if stop.parent_station.is_none() {
                    let stop_area = objects::StopArea::from(stop_point.clone());
                    stop_point.stop_area_id = stop_area.id.clone();
                    stop_areas.push(stop_area);
                };
                stop_point.comment_links = comment_links;
                stop_point.equipment_id = equipment_id;
                stop_points.push(stop_point);
            }
            StopLocationType::StopArea => {
                let mut stop_area =
                    skip_error_and_log!(objects::StopArea::try_from(stop), tracing::Level::WARN);
                stop_area.comment_links = comment_links;
                stop_area.equipment_id = equipment_id;
                stop_areas.push(stop_area);
            }
            _ => {
                let mut stop_location = skip_error_and_log!(
                    objects::StopLocation::try_from(stop),
                    tracing::Level::WARN
                );
                stop_location.comment_links = comment_links;
                stop_location.equipment_id = equipment_id;
                stop_locations.push(stop_location);
            }
        }
    }
    let stoppoints = CollectionWithId::new(stop_points)?;
    let stopareas = CollectionWithId::new(stop_areas)?;
    let stoplocations = CollectionWithId::new(stop_locations)?;
    Ok((stopareas, stoppoints, stoplocations))
}

pub(in crate::gtfs) fn manage_pathways<H>(
    collections: &mut Collections,
    file_handler: &mut H,
) -> Result<()>
where
    for<'a> &'a mut H: FileHandler,
{
    let file = "pathways.txt";

    let gtfs_pathways = read_objects_loose::<_, Pathway>(file_handler, file, false)?;
    let mut pathways = vec![];
    for mut pathway in gtfs_pathways {
        pathway.from_stop_type = skip_error_and_log!(
            collections
                .stop_points
                .get(&pathway.from_stop_id)
                .map(|st| st.stop_type.clone())
                .or_else(|| collections
                    .stop_locations
                    .get(&pathway.from_stop_id)
                    .map(|sl| sl.stop_type.clone()))
                .ok_or_else(|| {
                    format_err!(
                        "Problem reading {:?}: from_stop_id={:?} not found",
                        file,
                        pathway.from_stop_id
                    )
                }),
            tracing::Level::WARN
        );

        pathway.to_stop_type = skip_error_and_log!(
            collections
                .stop_points
                .get(&pathway.to_stop_id)
                .map(|st| st.stop_type.clone())
                .or_else(|| collections
                    .stop_locations
                    .get(&pathway.to_stop_id)
                    .map(|sl| sl.stop_type.clone()))
                .ok_or_else(|| {
                    format_err!(
                        "Problem reading {:?}: to_stop_id={:?} not found",
                        file,
                        pathway.to_stop_id
                    )
                }),
            tracing::Level::WARN
        );
        pathways.push(pathway);
    }
    collections.pathways = CollectionWithId::new(pathways)?;
    Ok(())
}

pub(in crate::gtfs) fn read_transfers<H>(
    file_handler: &mut H,
    stop_points: &CollectionWithId<objects::StopPoint>,
    stop_areas: &CollectionWithId<objects::StopArea>,
) -> Result<Collection<objects::Transfer>>
where
    for<'a> &'a mut H: FileHandler,
{
    let file = "transfers.txt";
    let gtfs_transfers = read_objects_loose::<_, Transfer>(file_handler, file, false)?;

    let mut transfers = vec![];
    for transfer in gtfs_transfers {
        let expand_stop_area = |stop_id: &str| -> Result<Vec<&StopPoint>> {
            if stop_areas.get(stop_id).is_some() {
                let list_stop_points = stop_points
                    .values()
                    .filter(|stop_point| stop_point.stop_area_id == stop_id)
                    .collect();
                Ok(list_stop_points)
            } else {
                stop_points
                    .get(stop_id)
                    .ok_or_else(|| {
                        format_err!(
                            "Problem reading {:?}: stop_id={:?} not found",
                            file,
                            stop_id
                        )
                    })
                    .map(|stop_point| vec![stop_point])
            }
        };
        let from_stop_points = skip_error_and_log!(
            expand_stop_area(transfer.from_stop_id.as_str()),
            tracing::Level::WARN
        );
        let to_stop_points = skip_error_and_log!(
            expand_stop_area(transfer.to_stop_id.as_str()),
            tracing::Level::WARN
        );
        for from_stop_point in &from_stop_points {
            let approx = from_stop_point.coord.approx();
            for to_stop_point in &to_stop_points {
                let (min_transfer_time, real_min_transfer_time) = match transfer.transfer_type {
                    TransferType::Recommended => {
                        let sq_distance = approx.sq_distance_to(&to_stop_point.coord);
                        let transfer_time = (sq_distance.sqrt() / 0.785) as u32;

                        (Some(transfer_time), Some(transfer_time + 2 * 60))
                    }
                    TransferType::Timed => (Some(0), Some(0)),
                    TransferType::WithTransferTime => {
                        if transfer.min_transfer_time.is_none() {
                            warn!(
                            "The min_transfer_time between from_stop_id {} and to_stop_id {} is empty",
                            from_stop_point.id, to_stop_point.id
                        );
                        }
                        (transfer.min_transfer_time, transfer.min_transfer_time)
                    }
                    TransferType::NotPossible => (Some(86400), Some(86400)),
                };

                transfers.push(objects::Transfer {
                    from_stop_id: from_stop_point.id.clone(),
                    to_stop_id: to_stop_point.id.clone(),
                    min_transfer_time,
                    real_min_transfer_time,
                    equipment_id: None,
                });
            }
        }
    }

    Ok(Collection::new(transfers))
}

fn get_commercial_mode(route_type: &RouteType) -> objects::CommercialMode {
    objects::CommercialMode {
        id: route_type.to_string(),
        name: match route_type {
            RouteType::CableCar => "Cable car".to_string(),
            RouteType::SuspendedCableCar => "Suspended cable car".to_string(),
            RouteType::UnknownMode => "Unknown mode".to_string(),
            RouteType::Air => "Airplane".to_string(),
            _ => route_type.to_string(),
        },
    }
}

fn get_physical_mode(route_type: &RouteType) -> objects::PhysicalMode {
    let repres = match route_type {
        RouteType::UnknownMode => "Bus".into(),
        RouteType::CableCar => "Funicular".into(),
        _ => route_type.to_string(),
    };
    objects::PhysicalMode {
        id: repres.clone(),
        name: repres,
        co2_emission: None,
    }
}

fn get_modes_from_gtfs(
    gtfs_routes: &CollectionWithId<Route>,
) -> (Vec<objects::CommercialMode>, Vec<objects::PhysicalMode>) {
    let gtfs_mode_types: BTreeSet<RouteType> =
        gtfs_routes.values().map(|r| r.route_type.clone()).collect();

    let commercial_modes = gtfs_mode_types
        .iter()
        .map(|mt| get_commercial_mode(mt))
        .collect();
    let physical_modes = gtfs_mode_types
        .iter()
        .map(|mt| get_physical_mode(mt))
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect();
    (commercial_modes, physical_modes)
}

fn get_route_with_smallest_name<'a>(routes: &'a [&Route]) -> &'a Route {
    routes.iter().min_by_key(|r| &r.id).unwrap()
}

type MapLineRoutes<'a> = BTreeMap<(Option<String>, String, Option<Idx<Route>>), Vec<&'a Route>>;

fn map_line_routes<'a>(
    gtfs_routes: &'a CollectionWithId<Route>,
    gtfs_trips: &[Trip],
    read_as_line: bool,
) -> MapLineRoutes<'a> {
    let mut map = BTreeMap::new();
    for (idx, r) in gtfs_routes.iter().filter(|(_, r)| {
        if !gtfs_trips.iter().any(|t| t.route_id == r.id) {
            warn!("Couldn't find trips for route_id {}", r.id);
            return false;
        }
        true
    }) {
        map.entry(r.generate_line_key(read_as_line, idx))
            .or_insert_with(Vec::new)
            .push(r);
    }
    map
}

fn make_lines(
    map_line_routes: &MapLineRoutes<'_>,
    networks: &CollectionWithId<objects::Network>,
) -> Result<Vec<objects::Line>> {
    let mut lines = vec![];

    let line_code = |r: &Route| {
        if r.short_name.is_empty() {
            None
        } else {
            Some(r.short_name.to_string())
        }
    };

    for routes in map_line_routes.values() {
        let r = get_route_with_smallest_name(routes);

        lines.push(objects::Line {
            id: r.id.clone(),
            code: line_code(r),
            codes: KeysValues::default(),
            object_properties: PropertiesMap::default(),
            comment_links: CommentLinksT::default(),
            name: r.long_name.to_string(),
            forward_name: None,
            forward_direction: None,
            backward_name: None,
            backward_direction: None,
            color: r.color.clone(),
            text_color: r.text_color.clone(),
            sort_order: r.sort_order,
            network_id: get_agency_id(r, networks)?,
            commercial_mode_id: r.route_type.to_string(),
            geometry_id: None,
            opening_time: None,
            closing_time: None,
        });
    }

    Ok(lines)
}

fn make_routes(gtfs_trips: &[Trip], map_line_routes: &MapLineRoutes<'_>) -> Vec<objects::Route> {
    let mut routes = vec![];

    let get_direction_name = |d: DirectionType| match d {
        DirectionType::Forward => "forward".to_string(),
        DirectionType::Backward => "backward".to_string(),
    };

    for rs in map_line_routes.values() {
        let sr = get_route_with_smallest_name(rs);
        for r in rs {
            let mut route_directions: BTreeSet<DirectionType> = BTreeSet::new();
            for t in gtfs_trips.iter().filter(|t| t.route_id == r.id) {
                route_directions.insert(t.direction);
            }

            let has_one_direction = route_directions.len() <= 1;
            for d in route_directions {
                routes.push(objects::Route {
                    id: r.get_id_by_direction(d),
                    // When only one direction, keep the route name. When
                    // multiple directions are possible, leave the `route_name`
                    // empty, it'll be auto-generated later in
                    // `Collections::enhance_route_names()`.
                    name: if has_one_direction {
                        if !r.long_name.is_empty() {
                            r.long_name.clone()
                        } else {
                            r.short_name.clone()
                        }
                    } else {
                        String::new()
                    },
                    direction_type: Some(get_direction_name(d)),
                    codes: KeysValues::default(),
                    object_properties: PropertiesMap::default(),
                    comment_links: CommentLinksT::default(),
                    line_id: sr.id.clone(),
                    geometry_id: None,
                    destination_id: None,
                });
            }
        }
    }
    routes
}

fn make_ntfs_vehicle_journeys(
    gtfs_trips: &[Trip],
    routes: &CollectionWithId<Route>,
    datasets: &CollectionWithId<objects::Dataset>,
    networks: &CollectionWithId<objects::Network>,
) -> (Vec<objects::VehicleJourney>, Vec<objects::TripProperty>) {
    // there always is one dataset from config or a default one
    let (_, dataset) = datasets.iter().next().unwrap();
    let mut vehicle_journeys: Vec<objects::VehicleJourney> = vec![];
    let mut trip_properties: Vec<objects::TripProperty> = vec![];
    let mut map_tps_trips: BTreeMap<(Availability, Availability), Vec<&Trip>> = BTreeMap::new();
    let mut id_incr: u8 = 1;
    let mut property_id: Option<String>;

    for t in gtfs_trips {
        map_tps_trips
            .entry((t.wheelchair_accessible, t.bikes_allowed))
            .or_insert_with(Vec::new)
            .push(t);
    }

    for ((wheelchair, bike), trips) in &map_tps_trips {
        if *wheelchair == Availability::InformationNotAvailable
            && *bike == Availability::InformationNotAvailable
        {
            property_id = None;
        } else {
            property_id = Some(id_incr.to_string());
            trip_properties.push(objects::TripProperty {
                id: id_incr.to_string(),
                wheelchair_accessible: *wheelchair,
                bike_accepted: *bike,
                air_conditioned: Availability::InformationNotAvailable,
                visual_announcement: Availability::InformationNotAvailable,
                audible_announcement: Availability::InformationNotAvailable,
                appropriate_escort: Availability::InformationNotAvailable,
                appropriate_signage: Availability::InformationNotAvailable,
                school_vehicle_type: TransportType::Regular,
            });
            id_incr += 1;
        }
        trips
            .iter()
            .map(|t| t.to_ntfs_vehicle_journey(routes, dataset, &property_id, networks))
            .skip_error_and_log(tracing::Level::WARN)
            .for_each(|vj| vehicle_journeys.push(vj));
    }

    (vehicle_journeys, trip_properties)
}

pub(in crate::gtfs) fn read_routes<H>(
    file_handler: &mut H,
    collections: &mut Collections,
    read_as_line: bool,
) -> Result<()>
where
    for<'a> &'a mut H: FileHandler,
{
    info!("Reading routes.txt");
    let gtfs_routes_collection = read_collection(file_handler, "routes.txt")?;
    let (commercial_modes, physical_modes) = get_modes_from_gtfs(&gtfs_routes_collection);
    collections.commercial_modes = CollectionWithId::new(commercial_modes)?;
    collections.physical_modes = CollectionWithId::new(physical_modes)?;

    let gtfs_trips = read_objects(file_handler, "trips.txt", true)?;
    let map_line_routes = map_line_routes(&gtfs_routes_collection, &gtfs_trips, read_as_line);
    let lines = make_lines(&map_line_routes, &collections.networks)?;
    collections.lines = CollectionWithId::new(lines)?;

    let routes = make_routes(&gtfs_trips, &map_line_routes);
    collections.routes = CollectionWithId::new(routes)?;

    gtfs_routes_collection.iter().for_each(|(_id, gtfs_route)| {
        if read_as_line {
            insert_comment(
                &mut collections.lines,
                &mut collections.comments,
                "line",
                &gtfs_route,
            );
        } else {
            insert_comment(
                &mut collections.routes,
                &mut collections.comments,
                "route",
                &gtfs_route,
            );
        };
    });

    let (vehicle_journeys, trip_properties) = make_ntfs_vehicle_journeys(
        &gtfs_trips,
        &gtfs_routes_collection,
        &collections.datasets,
        &collections.networks,
    );
    collections.vehicle_journeys = CollectionWithId::new(vehicle_journeys)?;
    collections.trip_properties = CollectionWithId::new(trip_properties)?;

    Ok(())
}

#[derive(Derivative, Deserialize, Debug, Clone, PartialEq)]
#[derivative(Default)]
enum FrequencyPrecision {
    #[derivative(Default)]
    #[serde(rename = "0")]
    Inexact,
    #[serde(rename = "1")]
    Exact,
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct Frequency {
    trip_id: String,
    start_time: Time,
    end_time: Time,
    headway_secs: u32,
    #[serde(default, deserialize_with = "de_with_empty_default")]
    exact_times: FrequencyPrecision,
}

pub(in crate::gtfs) fn manage_frequencies<H>(
    collections: &mut Collections,
    file_handler: &mut H,
) -> Result<()>
where
    for<'a> &'a mut H: FileHandler,
{
    let file = "frequencies.txt";
    let gtfs_frequencies = read_objects::<_, Frequency>(file_handler, file, false)?;
    let mut trip_id_sequence: HashMap<String, u32> = HashMap::new();
    let mut new_vehicle_journeys: Vec<VehicleJourney> = vec![];
    for frequency in &gtfs_frequencies {
        if frequency.start_time == frequency.end_time {
            warn!(
                "frequency for trip {:?} has same start and end time",
                frequency.trip_id
            );
            continue;
        }
        let datetime_estimated = match frequency.exact_times {
            FrequencyPrecision::Exact => false,
            FrequencyPrecision::Inexact => true,
        };
        let corresponding_vj = skip_error_and_log!(
            collections
                .vehicle_journeys
                .get(&frequency.trip_id)
                .cloned()
                .ok_or_else(|| format_err!(
                    "frequency mapped to an unexisting trip {:?}",
                    frequency.trip_id
                )),
            tracing::Level::WARN
        );
        let mut start_time = frequency.start_time;
        let mut arrival_time_delta = match corresponding_vj.stop_times.iter().min() {
            None => {
                warn!(
                    "frequency mapped to trip {:?} with no stop_times",
                    frequency.trip_id
                );
                continue;
            }
            Some(st) => st.arrival_time,
        };
        while start_time < frequency.end_time {
            trip_id_sequence
                .entry(frequency.trip_id.clone())
                .and_modify(|counter| *counter += 1)
                .or_insert(0);
            let generated_trip_id = format!(
                "{}-{}",
                frequency.trip_id, trip_id_sequence[&frequency.trip_id]
            );
            // the following handles generated trip starting after midnight, we need to generate a
            // new service in case the next day is not covered
            let service_id = if start_time.hours() >= 24 {
                let nb_days = start_time.hours() / 24;
                let service = collections
                    .calendars
                    .get(&corresponding_vj.service_id)
                    .cloned()
                    .unwrap();
                let new_service_id = format!("{}:+{}days", service.id, nb_days);
                if !collections.calendars.contains_id(&new_service_id) {
                    arrival_time_delta = arrival_time_delta + Time::new(24, 0, 0);
                    let new_dates: BTreeSet<_> = service
                        .dates
                        .iter()
                        .map(|d| *d + chrono::Duration::days(i64::from(nb_days)))
                        .collect();

                    let new_service = objects::Calendar {
                        id: new_service_id.clone(),
                        dates: new_dates,
                    };
                    collections.calendars.push(new_service)?;
                }
                new_service_id
            } else {
                corresponding_vj.service_id.clone()
            };
            let stop_times: Vec<NtfsStopTime> = corresponding_vj
                .stop_times
                .iter()
                .map(|stop_time| NtfsStopTime {
                    stop_point_idx: stop_time.stop_point_idx,
                    sequence: stop_time.sequence,
                    arrival_time: stop_time.arrival_time + start_time - arrival_time_delta,
                    departure_time: stop_time.departure_time + start_time - arrival_time_delta,
                    boarding_duration: stop_time.boarding_duration,
                    alighting_duration: stop_time.alighting_duration,
                    pickup_type: stop_time.pickup_type,
                    drop_off_type: stop_time.drop_off_type,
                    datetime_estimated,
                    local_zone_id: stop_time.local_zone_id,
                    precision: stop_time.precision.clone(),
                })
                .collect();
            start_time = start_time + Time::new(0, 0, frequency.headway_secs);
            let generated_vj = VehicleJourney {
                id: generated_trip_id.clone(),
                service_id,
                stop_times,
                ..corresponding_vj.clone()
            };
            new_vehicle_journeys.push(generated_vj);
            let stop_time_comments: HashMap<(String, u32), String> = corresponding_vj
                .stop_times
                .iter()
                .filter(|stop_time| stop_time.pickup_type == 2 || stop_time.drop_off_type == 2)
                .filter_map(|stop_time| {
                    collections
                        .stop_time_comments
                        .get(&(frequency.trip_id.clone(), stop_time.sequence))
                        .map(|comment_id| {
                            (
                                (generated_trip_id.clone(), stop_time.sequence),
                                comment_id.to_string(),
                            )
                        })
                })
                .collect();
            let stop_time_ids: HashMap<(String, u32), String> = stop_time_comments
                .keys()
                .map(|(trip_id, sequence)| {
                    (
                        (trip_id.to_string(), *sequence),
                        format!("{}-{}", trip_id, sequence),
                    )
                })
                .collect();
            collections.stop_time_comments.extend(stop_time_comments);
            collections.stop_time_ids.extend(stop_time_ids);
        }
    }
    let mut vehicle_journeys = collections.vehicle_journeys.take();
    let trip_ids_to_remove: Vec<_> = gtfs_frequencies.iter().map(|f| &f.trip_id).collect();
    vehicle_journeys.retain(|vj| !trip_ids_to_remove.contains(&&vj.id));
    collections
        .stop_time_ids
        .retain(|(vj_id, _), _| !trip_ids_to_remove.contains(&&vj_id));
    collections
        .stop_time_comments
        .retain(|(vj_id, _), _| !trip_ids_to_remove.contains(&&vj_id));

    vehicle_journeys.append(&mut new_vehicle_journeys);
    collections.vehicle_journeys = CollectionWithId::new(vehicle_journeys)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        calendars,
        gtfs::read::EquipmentList,
        model::Collections,
        objects::*,
        objects::{Calendar, Comment, CommentType, Equipment, Geometry, Rgb, StopTime, Transfer},
        read_utils::{self, read_opt_collection, PathFileHandler},
        test_utils::*,
        AddPrefix, PrefixConfiguration,
    };
    use geo::line_string;
    use pretty_assertions::assert_eq;
    use typed_index_collection::Id;

    fn extract<'a, T, S: ::std::cmp::Ord>(f: fn(&'a T) -> S, c: &'a Collection<T>) -> Vec<S> {
        let mut extracted_props: Vec<S> = c.values().map(|l| f(l)).collect();
        extracted_props.sort();
        extracted_props
    }

    fn extract_ids<T: Id<T>>(c: &Collection<T>) -> Vec<&str> {
        extract(T::id, c)
    }

    #[test]
    fn load_minimal_agency() {
        let agency_content = "agency_name,agency_url,agency_timezone\n\
                              My agency,http://my-agency_url.com,Europe/London";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "agency.txt", agency_content);
            let (networks, companies) = super::read_agency(&mut handler).unwrap();
            assert_eq!(1, networks.len());
            let agency = networks.iter().next().unwrap().1;
            assert_eq!("1", agency.id);
            assert_eq!(1, companies.len());
        });
    }

    #[test]
    fn load_standard_agency() {
        let agency_content = "agency_id,agency_name,agency_url,agency_timezone\n\
                              id_1,My agency,http://my-agency_url.com,Europe/London";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "agency.txt", agency_content);
            let (networks, companies) = super::read_agency(&mut handler).unwrap();
            assert_eq!(1, networks.len());
            assert_eq!(1, companies.len());
        });
    }

    #[test]
    fn load_complete_agency() {
        let agency_content =
            "agency_id,agency_name,agency_url,agency_timezone,agency_lang,agency_phone,\
             agency_fare_url,agency_email\n\
             id_1,My agency,http://my-agency_url.com,Europe/London,EN,0123456789,\
             http://my-agency_fare_url.com,my-mail@example.com";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "agency.txt", agency_content);
            let (networks, companies) = super::read_agency(&mut handler).unwrap();
            assert_eq!(1, networks.len());
            let network = networks.iter().next().unwrap().1;
            assert_eq!("id_1", network.id);
            assert_eq!(1, companies.len());
        });
    }

    #[test]
    #[should_panic(
        expected = "IdentifierAlreadyExists Error { id: \"1\", type: \"transit_model::objects::Network\" }"
    )]
    fn load_2_agencies_with_no_id() {
        let agency_content = "agency_name,agency_url,agency_timezone\n\
                              My agency 1,http://my-agency_url.com,Europe/London\n\
                              My agency 2,http://my-agency_url.com,Europe/London";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "agency.txt", agency_content);
            super::read_agency(&mut handler).unwrap();
        });
    }

    #[test]
    fn load_2_agencies_with_different_timezone() {
        let agency_content = "agency_id,agency_name,agency_url,agency_timezone\n\
                              id_1,My agency 1,http://my-agency_url.com,Europe/London\n\
                              id_2,My agency 2,http://my-agency_url.com,Europe/Paris";

        test_in_tmp_dir(|path| {
            testing_logger::setup();
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "agency.txt", agency_content);
            super::read_agency(&mut handler).unwrap();
            testing_logger::validate(|captured_logs| {
                assert_eq!(captured_logs.len(), 2);
                assert_eq!(
                    captured_logs[1].body,
                    "different agency timezone: Europe/London (id_1) - Europe/Paris (id_2)"
                );
                assert_eq!(captured_logs[1].level, log::Level::Warn);
            });
        });
    }

    #[test]
    fn load_one_stop_point() {
        let stops_content = "stop_id,stop_name,stop_code,stop_lat,stop_lon\n\
                             id1,my stop name,stopcode,0.1,1.2";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "stops.txt", stops_content);
            let mut equipments = EquipmentList::default();
            let mut comments: CollectionWithId<Comment> = CollectionWithId::default();

            let (stop_areas, stop_points, stop_locations) =
                super::read_stops(&mut handler, &mut comments, &mut equipments).unwrap();
            assert_eq!(1, stop_areas.len());
            assert_eq!(1, stop_points.len());
            assert_eq!(0, stop_locations.len());
            let stop_area = stop_areas.iter().next().unwrap().1;
            assert_eq!("Navitia:id1", stop_area.id);

            assert_eq!(1, stop_points.len());
            let stop_point = stop_points.iter().next().unwrap().1;
            assert_eq!("Navitia:id1", stop_point.stop_area_id);
            assert_eq!("stopcode", stop_point.code.as_ref().unwrap());
        });
    }

    #[test]
    fn load_without_slashes() {
        let stops_content = "stop_id,stop_name,stop_lat,stop_lon,location_type,parent_station\n\
                             stoparea/01,my stop name 1,0.1,1.2,1,\n\
                             stoppoint/01,my stop name 2,0.1,1.2,0,stoparea/01\n\
                             stoppoint/02,my stop name 3,0.1,1.2,0,stoparea/01";
        let shapes_content =
            "shape_id,shape_pt_lat,shape_pt_lon,shape_pt_sequence,shape_dist_traveled\n\
             relation/1,12.1280176,-86.214164,1,\n\
             relation/1,12.1279272,-86.2132786,2,";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "stops.txt", stops_content);
            create_file_with_content(path, "shapes.txt", shapes_content);
            let mut collections = Collections::default();
            let mut equipments = EquipmentList::default();
            let mut comments: CollectionWithId<Comment> = CollectionWithId::default();
            // let stop_file = File::open(path.join("stops.txt")).unwrap();
            let (stop_areas, stop_points, stop_locations) =
                super::read_stops(&mut handler, &mut comments, &mut equipments).unwrap();
            collections.stop_areas = stop_areas;
            collections.stop_points = stop_points;
            collections.stop_locations = stop_locations;
            super::manage_shapes(&mut collections, &mut handler).unwrap();
            let stop_area = collections.stop_areas.iter().next().unwrap().1;
            assert_eq!("stoparea01", stop_area.id);
            assert_eq!(
                vec![("stoppoint01", "stoparea01"), ("stoppoint02", "stoparea01"),],
                extract(
                    |sp| (sp.id.as_str(), sp.stop_area_id.as_str()),
                    &collections.stop_points
                )
            );

            assert_eq!(
                vec!["relation1"],
                extract(|geo| geo.id.as_str(), &collections.geometries)
            );
        });
    }

    #[test]
    fn stop_code_on_stops() {
        let stops_content =
            "stop_id,stop_code,stop_name,stop_lat,stop_lon,location_type,parent_station\n\
             stoppoint_id,1234,my stop name,0.1,1.2,0,stop_area_id\n\
             stoparea_id,5678,stop area name,0.1,1.2,1,";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "stops.txt", stops_content);
            let mut equipments = EquipmentList::default();
            let mut comments: CollectionWithId<Comment> = CollectionWithId::default();
            let (stop_areas, stop_points, stop_locations) =
                super::read_stops(&mut handler, &mut comments, &mut equipments).unwrap();
            //validate stop_point code
            assert_eq!(1, stop_points.len());
            let stop_point = stop_points.iter().next().unwrap().1;
            assert_eq!("1234", stop_point.code.as_ref().unwrap());
            assert_eq!(2, stop_point.codes.len());
            let mut codes_iterator = stop_point.codes.iter();
            let code = codes_iterator.next().unwrap();
            assert_eq!("gtfs_stop_code", code.0);
            assert_eq!("1234", code.1);
            let code = codes_iterator.next().unwrap();
            assert_eq!("source", code.0);
            assert_eq!("stoppoint_id", code.1);

            //validate stop_area code
            assert_eq!(1, stop_areas.len());
            let stop_area = stop_areas.iter().next().unwrap().1;
            assert_eq!(2, stop_area.codes.len());
            let mut codes_iterator = stop_area.codes.iter();
            let code = codes_iterator.next().unwrap();
            assert_eq!("gtfs_stop_code", code.0);
            assert_eq!("5678", code.1);
            let code = codes_iterator.next().unwrap();
            assert_eq!("source", code.0);
            assert_eq!("stoparea_id", code.1);
            assert_eq!(0, stop_locations.len());
        });
    }

    #[test]
    fn no_stop_code_on_autogenerated_stoparea() {
        let stops_content =
            "stop_id,stop_code,stop_name,stop_lat,stop_lon,location_type,parent_station\n\
             stoppoint_id,1234,my stop name,0.1,1.2,0,";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "stops.txt", stops_content);
            let mut equipments = EquipmentList::default();
            let mut comments: CollectionWithId<Comment> = CollectionWithId::default();
            let (stop_areas, _, _) =
                super::read_stops(&mut handler, &mut comments, &mut equipments).unwrap();
            //validate stop_area code
            assert_eq!(1, stop_areas.len());
            let stop_area = stop_areas.iter().next().unwrap().1;
            assert_eq!(0, stop_area.codes.len());
        });
    }

    #[test]
    fn gtfs_routes_as_line() {
        let routes_content = "route_id,agency_id,route_short_name,route_long_name,route_type,route_color,route_text_color\n\
                              route_1,agency_1,1,My line 1,3,8F7A32,FFFFFF\n\
                              route_2,agency_2,,My line 2,2,7BC142,000000\n\
                              route_3,agency_3,3,My line 3,8,,\n\
                              route_4,agency_4,3,My line 3 for agency 3,8,,";

        let trips_content =
            "trip_id,route_id,direction_id,service_id,wheelchair_accessible,bikes_allowed\n\
             1,route_1,,service_1,,\n\
             2,route_1,1,service_1,,\n\
             3,route_2,0,service_2,,\n\
             4,route_3,0,service_3,,\n\
             5,route_4,0,service_4,,";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);
            let mut collections = Collections::default();
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();
            super::read_routes(&mut handler, &mut collections, false).unwrap();
            assert_eq!(4, collections.lines.len());
            assert_eq!(
                vec!["agency_1", "agency_2", "agency_3", "agency_4"],
                extract(|l| &l.network_id, &collections.lines)
            );
            assert_eq!(3, collections.commercial_modes.len());

            assert_eq!(
                vec!["Bus", "Train", "Unknown mode"],
                extract(|cm| &cm.name, &collections.commercial_modes)
            );

            let lines_commercial_modes_id: Vec<String> = collections
                .lines
                .values()
                .map(|l| l.commercial_mode_id.clone())
                .collect();
            assert!(lines_commercial_modes_id.contains(&"Train".to_string()));
            assert!(lines_commercial_modes_id.contains(&"Bus".to_string()));
            assert!(lines_commercial_modes_id.contains(&"UnknownMode".to_string()));

            assert_eq!(2, collections.physical_modes.len());
            assert_eq!(
                vec!["Bus", "Train"],
                extract(|pm| &pm.name, &collections.physical_modes)
            );

            assert_eq!(5, collections.routes.len());

            assert_eq!(
                vec!["route_1", "route_1_R", "route_2", "route_3", "route_4"],
                extract_ids(&collections.routes)
            );
        });
    }

    #[test]
    fn gtfs_routes_without_agency_id_as_line() {
        let agency_content = "agency_id,agency_name,agency_url,agency_timezone\n\
                              id_agency,My agency,http://my-agency_url.com,Europe/London";

        let routes_content =
            "route_id,route_short_name,route_long_name,route_type,route_color,route_text_color\n\
             route_1,1,My line 1,3,8F7A32,FFFFFF\n\
             route_2,,My line 2,2,7BC142,000000\n\
             route_3,3,My line 3,8,,\n\
             route_4,3,My line 3 for agency 3,8,,";

        let trips_content =
            "trip_id,route_id,direction_id,service_id,wheelchair_accessible,bikes_allowed\n\
             1,route_1,,service_1,,\n\
             2,route_1,1,service_1,,\n\
             3,route_2,0,service_2,,\n\
             4,route_3,0,service_3,,\n\
             5,route_4,0,service_4,,";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "agency.txt", agency_content);
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);

            let mut collections = Collections::default();
            let (networks, _) = super::read_agency(&mut handler).unwrap();
            collections.networks = networks;
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();
            super::read_routes(&mut handler, &mut collections, false).unwrap();
            assert_eq!(3, collections.lines.len());

            assert_eq!(5, collections.routes.len());

            assert_eq!(
                vec!["id_agency", "id_agency", "id_agency"],
                extract(|l| &l.network_id, &collections.lines)
            );
        });
    }

    #[test]
    fn gtfs_routes_with_wrong_colors() {
        let agency_content = "agency_id,agency_name,agency_url,agency_timezone\n\
                              id_agency1,My agency 1,http://my-agency_url1.com,Europe/London";
        let routes_content =
            "route_id,route_short_name,route_long_name,route_type,route_color,route_text_color\n\
             route_1,1,My line 1,3,0,FFFFFF\n\
             route_2,,My line 2,2,7BC142,0\n\
             route_3,3,My line 3,8,FFFFFF,000000\n\
             route_4,3,My line 3 for agency 3,8,FFFFAF,FAFFFF";

        let trips_content =
            "trip_id,route_id,direction_id,service_id,wheelchair_accessible,bikes_allowed\n\
             1,route_1,,service_1,,\n\
             2,route_1,1,service_1,,\n\
             3,route_2,0,service_2,,\n\
             4,route_3,0,service_3,,\n\
             5,route_4,0,service_4,,";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "agency.txt", agency_content);
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);

            let mut collections = Collections::default();
            let (networks, _) = super::read_agency(&mut handler).unwrap();
            collections.networks = networks;
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();
            super::read_routes(&mut handler, &mut collections, false).unwrap();
            assert_eq!(3, collections.lines.len());
            assert_eq!(
                vec![
                    (
                        &None,
                        &Some(Rgb {
                            red: 255,
                            green: 255,
                            blue: 255
                        })
                    ),
                    (
                        &Some(Rgb {
                            red: 123,
                            green: 193,
                            blue: 66
                        }),
                        &None
                    ),
                    (
                        &Some(Rgb {
                            red: 255,
                            green: 255,
                            blue: 255
                        }),
                        &Some(Rgb {
                            red: 0,
                            green: 0,
                            blue: 0
                        })
                    )
                ],
                extract(|l| (&l.color, &l.text_color), &collections.lines)
            );
        });
    }

    #[test]
    #[should_panic(expected = "Impossible to get agency id, several networks found")]
    fn gtfs_routes_without_agency_id_as_line_and_2_agencies() {
        let agency_content = "agency_id,agency_name,agency_url,agency_timezone\n\
                              id_agency1,My agency 1,http://my-agency_url1.com,Europe/London\n\
                              id_agency2,My agency 2,http://my-agency_url2.com,Europe/London";

        let routes_content =
            "route_id,route_short_name,route_long_name,route_type,route_color,route_text_color\n\
             route_1,1,My line 1,3,8F7A32,FFFFFF\n\
             route_2,,My line 2,2,7BC142,000000\n\
             route_3,3,My line 3,8,,\n\
             route_4,3,My line 3 for agency 3,8,,";

        let trips_content =
            "trip_id,route_id,direction_id,service_id,wheelchair_accessible,bikes_allowed\n\
             1,route_1,,service_1,,\n\
             2,route_1,1,service_1,,\n\
             3,route_2,0,service_2,,\n\
             4,route_3,0,service_3,,\n\
             5,route_4,0,service_4,,";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "agency.txt", agency_content);
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);

            let mut collections = Collections::default();
            let (networks, _) = super::read_agency(&mut handler).unwrap();
            collections.networks = networks;
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();
            super::read_routes(&mut handler, &mut collections, false).unwrap();
        });
    }

    #[test]
    #[should_panic(expected = "Impossible to get agency id, no network found")]
    fn gtfs_routes_without_agency_id_as_line_and_0_agencies() {
        let routes_content =
            "route_id,route_short_name,route_long_name,route_type,route_color,route_text_color\n\
             route_1,1,My line 1,3,8F7A32,FFFFFF\n\
             route_2,,My line 2,2,7BC142,000000\n\
             route_3,3,My line 3,8,,\n\
             route_4,3,My line 3 for agency 3,8,,";

        let trips_content =
            "trip_id,route_id,direction_id,service_id,wheelchair_accessible,bikes_allowed\n\
             1,route_1,,service_1,,\n\
             2,route_1,1,service_1,,\n\
             3,route_2,0,service_2,,\n\
             4,route_3,0,service_3,,\n\
             5,route_4,0,service_4,,";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);

            let mut collections = Collections::default();
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();
            super::read_routes(&mut handler, &mut collections, false).unwrap();
        });
    }

    #[test]
    fn gtfs_routes_as_route() {
        let agency_content = "agency_id,agency_name,agency_url,agency_timezone\n\
                              id_agency,My agency,http://my-agency_url.com,Europe/London";

        let routes_content = "route_id,agency_id,route_short_name,route_long_name,route_type,route_color,route_text_color\n\
                              route_1,agency_1,1,My line 1A,3,8F7A32,FFFFFF\n\
                              route_2,agency_1,1,My line 1B,3,8F7A32,FFFFFF\n\
                              route_4,agency_2,1,My line 1B,3,8F7A32,FFFFFF\n\
                              route_3,agency_2,1,My line 1B,3,8F7A32,FFFFFF\n\
                              route_5,,1,My line 1C,3,8F7A32,FFFFFF";

        let trips_content =
            "trip_id,route_id,direction_id,service_id,wheelchair_accessible,bikes_allowed\n\
             1,route_1,0,service_1,,\n\
             2,route_2,0,service_1,,\n\
             3,route_3,0,service_2,,\n\
             4,route_4,0,service_2,,\n\
             5,route_5,0,service_3,,";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "agency.txt", agency_content);
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);
            let mut collections = Collections::default();
            let (networks, _) = super::read_agency(&mut handler).unwrap();
            collections.networks = networks;
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();
            super::read_routes(&mut handler, &mut collections, false).unwrap();

            assert_eq!(3, collections.lines.len());
            assert_eq!(
                vec!["agency_1", "agency_2", "id_agency"],
                extract(|l| &l.network_id, &collections.lines)
            );
            assert_eq!(
                vec!["route_1", "route_3", "route_5"],
                extract_ids(&collections.lines)
            );
            assert_eq!(5, collections.routes.len());

            assert_eq!(
                vec!["route_1", "route_1", "route_3", "route_3", "route_5"],
                extract(|r| &r.line_id, &collections.routes)
            );
        });
    }

    #[test]
    fn gtfs_routes_as_route_with_backward_trips() {
        let routes_content = "route_id,agency_id,route_short_name,route_long_name,route_type,route_color,route_text_color\n\
                              route_1,agency_1,1,My line 1A,3,8F7A32,FFFFFF\n\
                              route_2,agency_1,1,My line 1B,3,8F7A32,FFFFFF\n\
                              route_3,agency_2,,My line 2,2,7BC142,000000";

        let trips_content =
            "trip_id,route_id,direction_id,service_id,wheelchair_accessible,bikes_allowed\n\
             1,route_1,0,service_1,,\n\
             2,route_1,1,service_1,,\n\
             3,route_2,0,service_2,,\n
             4,route_3,0,service_3,,\n\
             5,route_3,1,service_3,,";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);
            let mut collections = Collections::default();
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();
            super::read_routes(&mut handler, &mut collections, false).unwrap();

            assert_eq!(2, collections.lines.len());

            assert_eq!(5, collections.routes.len());
            assert_eq!(
                vec!["route_1", "route_1_R", "route_2", "route_3", "route_3_R"],
                extract_ids(&collections.routes)
            );
        });
    }

    #[test]
    fn gtfs_routes_as_route_same_name_different_agency() {
        let routes_content = "route_id,agency_id,route_short_name,route_long_name,route_type,route_color,route_text_color\n\
                              route_1,agency_1,1,My line 1A,3,8F7A32,FFFFFF\n\
                              route_2,agency_1,1,My line 1B,3,8F7A32,FFFFFF\n\
                              route_3,agency_2,1,My line 1 for agency 2,3,8F7A32,FFFFFF";

        let trips_content =
            "trip_id,route_id,direction_id,service_id,wheelchair_accessible,bikes_allowed\n\
             1,route_1,0,service_1,,\n\
             2,route_2,0,service_2,,\n
             3,route_3,0,service_3,,";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);
            let mut collections = Collections::default();
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();
            super::read_routes(&mut handler, &mut collections, false).unwrap();

            assert_eq!(2, collections.lines.len());
            assert_eq!(vec!["route_1", "route_3"], extract_ids(&collections.lines));
            assert_eq!(
                vec!["route_1", "route_2", "route_3"],
                extract_ids(&collections.routes)
            );

            assert_eq!(
                vec!["route_1", "route_1", "route_3"],
                extract(|r| &r.line_id, &collections.routes)
            );
        });
    }

    #[test]
    fn gtfs_routes_with_no_trips() {
        let routes_content = "route_id,agency_id,route_short_name,route_long_name,route_type,route_color,route_text_color\n\
                              route_1,agency_1,1,My line 1,3,8F7A32,FFFFFF\n\
                              route_2,agency_2,2,My line 2,3,8F7A32,FFFFFF";
        let trips_content =
            "trip_id,route_id,direction_id,service_id,wheelchair_accessible,bikes_allowed\n\
             1,route_1,0,service_1,,";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);

            let mut collections = Collections::default();
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();
            super::read_routes(&mut handler, &mut collections, false).unwrap();
            assert_eq!(1, collections.lines.len());
            assert_eq!(1, collections.routes.len());
        });
    }

    #[test]
    fn prefix_on_all_pt_object_id() {
        let stops_content =
            "stop_id,stop_name,stop_desc,stop_lat,stop_lon,location_type,parent_station,wheelchair_boarding\n\
             sp:01,my stop point name,my first desc,0.1,1.2,0,,1\n\
             sp:02,my stop point name child,,0.2,1.5,0,sp:01,2\n\
             sa:03,my stop area name,my second desc,0.3,2.2,1,,1";
        let agency_content = "agency_id,agency_name,agency_url,agency_timezone,agency_lang\n\
                              584,TAM,http://whatever.canaltp.fr/,Europe/Paris,fr\n\
                              285,Phébus,http://plop.kisio.com/,Europe/London,en";

        let routes_content = "route_id,agency_id,route_short_name,route_long_name,route_type,route_color,route_text_color,destination_id\n\
                              route_1,agency_1,1,My line 1A,3,8F7A32,FFFFFF,\n\
                              route_2,agency_1,2,My line 1B,3,8F7A32,FFFFFF,sp:01";

        let trips_content =
            "trip_id,route_id,direction_id,service_id,wheelchair_accessible,bikes_allowed,shape_id\n\
             1,route_1,0,service_1,,,1\n\
             2,route_2,1,service_2,1,2,2";

        let transfers_content = "from_stop_id,to_stop_id,transfer_type,min_transfer_time\n\
                                 sp:01,sp:01,1,\n\
                                 sp:01,sp:02,0,\n\
                                 sp:02,sp:01,0,\n\
                                 sp:02,sp:02,1,";

        let shapes_content = "shape_id,shape_pt_lat,shape_pt_lon,shape_pt_sequence\n\
                              1,4.4,3.3,2\n\
                              2,6.6,5.5,1";

        let calendar = "service_id,monday,tuesday,wednesday,thursday,friday,saturday,sunday,start_date,end_date\n\
                       1,0,0,0,0,0,1,1,20180501,20180508\n\
                       2,1,0,0,0,0,0,0,20180502,20180506";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "stops.txt", stops_content);
            create_file_with_content(path, "agency.txt", agency_content);
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);
            create_file_with_content(path, "transfers.txt", transfers_content);
            create_file_with_content(path, "shapes.txt", shapes_content);
            create_file_with_content(path, "calendar.txt", calendar);

            let mut collections = Collections::default();

            let mut comments: CollectionWithId<Comment> = CollectionWithId::default();
            let mut equipments = EquipmentList::default();
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();
            let (stop_areas, stop_points, stop_locations) =
                super::read_stops(&mut handler, &mut comments, &mut equipments).unwrap();
            collections.equipments = CollectionWithId::new(equipments.into_equipments()).unwrap();
            collections.transfers =
                super::read_transfers(&mut handler, &stop_points, &stop_areas).unwrap();
            collections.stop_areas = stop_areas;
            collections.stop_points = stop_points;
            collections.stop_locations = stop_locations;

            let (networks, companies) = super::read_agency(&mut handler).unwrap();
            collections.networks = networks;
            collections.companies = companies;
            collections.comments = comments;
            super::read_routes(&mut handler, &mut collections, false).unwrap();
            super::manage_shapes(&mut collections, &mut handler).unwrap();
            calendars::manage_calendars(&mut handler, &mut collections).unwrap();

            let mut prefix_conf = PrefixConfiguration::default();
            prefix_conf.set_data_prefix("my_prefix");
            collections.prefix(&prefix_conf);

            assert_eq!(
                vec!["my_prefix:285", "my_prefix:584"],
                extract_ids(&collections.companies)
            );
            assert_eq!(
                vec!["my_prefix:285", "my_prefix:584"],
                extract_ids(&collections.networks)
            );
            assert_eq!(
                vec![
                    ("my_prefix:Navitia:sp:01", None),
                    ("my_prefix:sa:03", Some("my_prefix:0")),
                ],
                extract(
                    |obj| (obj.id.as_str(), obj.equipment_id.as_deref()),
                    &collections.stop_areas,
                )
            );
            assert_eq!(
                vec![
                    (
                        "my_prefix:sp:01",
                        "my_prefix:Navitia:sp:01",
                        Some("my_prefix:0")
                    ),
                    ("my_prefix:sp:02", "my_prefix:sp:01", Some("my_prefix:1")),
                ],
                extract(
                    |obj| (
                        obj.id.as_str(),
                        obj.stop_area_id.as_str(),
                        obj.equipment_id.as_deref()
                    ),
                    &collections.stop_points,
                )
            );
            assert_eq!(
                vec![
                    ("my_prefix:route_1", "my_prefix:agency_1", "Bus", None, None),
                    ("my_prefix:route_2", "my_prefix:agency_1", "Bus", None, None),
                ],
                extract(
                    |obj| (
                        obj.id.as_str(),
                        obj.network_id.as_str(),
                        obj.commercial_mode_id.as_str(),
                        obj.forward_direction.as_deref(),
                        obj.backward_direction.as_deref(),
                    ),
                    &collections.lines,
                )
            );
            assert_eq!(
                vec![
                    ("my_prefix:route_1", "my_prefix:route_1", None),
                    ("my_prefix:route_2_R", "my_prefix:route_2", None),
                ],
                extract(
                    |obj| (
                        obj.id.as_str(),
                        obj.line_id.as_str(),
                        obj.destination_id.as_deref()
                    ),
                    &collections.routes,
                )
            );
            assert_eq!(
                vec!["my_prefix:1"],
                extract_ids(&collections.trip_properties)
            );
            assert_eq!(
                vec!["my_prefix:stop:sa:03", "my_prefix:stop:sp:01"],
                extract_ids(&collections.comments)
            );
            assert_eq!(vec!["Bus"], extract_ids(&collections.commercial_modes));
            assert_eq!(
                vec![
                    ("my_prefix:sp:01", "my_prefix:sp:01"),
                    ("my_prefix:sp:01", "my_prefix:sp:02"),
                    ("my_prefix:sp:02", "my_prefix:sp:01"),
                    ("my_prefix:sp:02", "my_prefix:sp:02"),
                ],
                extract(
                    |sp| (sp.from_stop_id.as_str(), sp.to_stop_id.as_str()),
                    &collections.transfers,
                )
            );
            assert_eq!(
                vec!["my_prefix:default_contributor"],
                extract_ids(&collections.contributors)
            );
            assert_eq!(
                vec![("my_prefix:default_dataset", "my_prefix:default_contributor")],
                extract(
                    |obj| (obj.id.as_str(), obj.contributor_id.as_str()),
                    &collections.datasets,
                )
            );
            assert_eq!(
                vec![
                    (
                        "my_prefix:1",
                        "my_prefix:route_1",
                        "my_prefix:default_dataset",
                        "my_prefix:service_1",
                        Some("my_prefix:1"),
                    ),
                    (
                        "my_prefix:2",
                        "my_prefix:route_2_R",
                        "my_prefix:default_dataset",
                        "my_prefix:service_2",
                        Some("my_prefix:2"),
                    ),
                ],
                extract(
                    |obj| (
                        obj.id.as_str(),
                        obj.route_id.as_str(),
                        obj.dataset_id.as_str(),
                        obj.service_id.as_str(),
                        obj.geometry_id.as_deref()
                    ),
                    &collections.vehicle_journeys,
                )
            );
            assert_eq!(
                vec!["my_prefix:0", "my_prefix:1"],
                extract_ids(&collections.equipments)
            );
            assert_eq!(
                vec!["my_prefix:1", "my_prefix:2"],
                extract_ids(&collections.geometries)
            );
            assert_eq!(vec!["my_prefix:1"], extract_ids(&collections.calendars));
        });
    }

    #[test]
    fn gtfs_trips() {
        let routes_content = "route_id,agency_id,route_short_name,route_long_name,route_type,route_color,route_text_color\n\
                              route_1,agency_1,1,My line 1,3,8F7A32,FFFFFF\n\
                              route_2,agency_2,2,My line 2,3,8F7A32,FFFFFF\n\
                              route_3,agency_3,3,My line 3,3,8F7A32,FFFFFF";
        let trips_content =
            "trip_id,route_id,direction_id,service_id,wheelchair_accessible,bikes_allowed\n\
             1,route_1,0,service_1,,\n\
             2,route_2,0,service_1,1,2\n\
             3,route_3,0,service_1,1,2
             4,unknown_route,0,service_1,1,2";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);

            let mut collections = Collections::default();
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();

            super::read_routes(&mut handler, &mut collections, false).unwrap();
            assert_eq!(3, collections.lines.len());
            assert_eq!(3, collections.routes.len());
            assert_eq!(3, collections.vehicle_journeys.len());
            assert_eq!(
                vec!["agency_1", "agency_2", "agency_3"],
                extract(|vj| &vj.company_id, &collections.vehicle_journeys)
            );
            assert_eq!(1, collections.trip_properties.len());
        });
    }

    #[test]
    fn gtfs_trips_with_routes_without_agency_id() {
        let agency_content = "agency_id,agency_name,agency_url,agency_timezone\n\
                              id_agency,My agency,http://my-agency_url.com,Europe/London";

        let routes_content =
            "route_id,route_short_name,route_long_name,route_type,route_color,route_text_color\n\
             route_1,1,My line 1,3,8F7A32,FFFFFF\n\
             route_2,2,My line 2,3,8F7A32,FFFFFF\n\
             route_3,3,My line 3,3,8F7A32,FFFFFF";
        let trips_content =
            "trip_id,route_id,direction_id,service_id,wheelchair_accessible,bikes_allowed\n\
             1,route_1,0,service_1,,\n\
             2,route_2,0,service_1,1,2\n\
             3,route_3,0,service_1,1,2";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "agency.txt", agency_content);
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);

            let mut collections = Collections::default();
            let (networks, _) = super::read_agency(&mut handler).unwrap();
            collections.networks = networks;
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();

            super::read_routes(&mut handler, &mut collections, false).unwrap();
            assert_eq!(3, collections.lines.len());
            assert_eq!(3, collections.routes.len());
            assert_eq!(3, collections.vehicle_journeys.len());
            assert_eq!(
                vec!["id_agency", "id_agency", "id_agency"],
                extract(|vj| &vj.company_id, &collections.vehicle_journeys)
            );
            assert_eq!(1, collections.trip_properties.len());
        });
    }

    #[test]
    fn gtfs_trips_no_direction_id() {
        let routes_content = "route_id,agency_id,route_short_name,route_long_name,route_type,route_color,route_text_color\n\
                              route_1,agency_1,1,My line 1,3,8F7A32,FFFFFF\n\
                              route_2,agency_2,2,My line 2,3,8F7A32,FFFFFF\n\
                              route_3,agency_3,3,My line 3,3,8F7A32,FFFFFF";
        let trips_content = "trip_id,route_id,service_id,wheelchair_accessible,bikes_allowed\n\
                             1,route_1,service_1,,\n\
                             2,route_2,service_1,1,2\n\
                             3,route_3,service_1,1,2";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);

            let mut collections = Collections::default();
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();

            super::read_routes(&mut handler, &mut collections, false).unwrap();
            assert_eq!(3, collections.lines.len());
            assert_eq!(3, collections.routes.len());

            assert_eq!(
                vec![
                    &Some("forward".to_string()),
                    &Some("forward".to_string()),
                    &Some("forward".to_string())
                ],
                extract(|r| &r.direction_type, &collections.routes)
            );
        });
    }

    #[test]
    fn gtfs_trips_with_no_accessibility_information() {
        let routes_content = "route_id,agency_id,route_short_name,route_long_name,route_type,route_color,route_text_color\n\
                              route_1,agency_1,1,My line 1,3,8F7A32,FFFFFF";
        let trips_content =
            "trip_id,route_id,direction_id,service_id,wheelchair_accessible,bikes_allowed\n\
             1,route_1,0,service_1,,\n\
             2,route_1,0,service_2,,";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);

            let mut collections = Collections::default();
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();

            super::read_routes(&mut handler, &mut collections, false).unwrap();
            assert_eq!(2, collections.vehicle_journeys.len());
            assert_eq!(0, collections.trip_properties.len());
            for vj in collections.vehicle_journeys.values() {
                assert!(vj.trip_property_id.is_none());
            }
        });
    }

    #[test]
    fn push_on_collection() {
        let mut c = CollectionWithId::default();
        c.push(Comment {
            id: "foo".into(),
            name: "toto".into(),
            comment_type: CommentType::Information,
            url: None,
            label: None,
        })
        .unwrap();
        assert!(c
            .push(Comment {
                id: "foo".into(),
                name: "tata".into(),
                comment_type: CommentType::Information,
                url: None,
                label: None,
            })
            .is_err());
        let id = c.get_idx("foo").unwrap();
        assert_eq!(c.iter().next().unwrap().0, id);
    }

    #[test]
    fn stops_generates_equipments() {
        let stops_content = "stop_id,stop_name,stop_lat,stop_lon,location_type,parent_station,wheelchair_boarding\n\
                             sp:01,my stop point name,0.1,1.2,0,,1\n\
                             sp:02,my stop point name child,0.2,1.5,0,sp:01,\n\
                             sa:03,my stop area name,0.3,2.2,1,,2";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "stops.txt", stops_content);

            let mut comments: CollectionWithId<Comment> = CollectionWithId::default();
            let mut equipments = EquipmentList::default();
            let (stop_areas, stop_points, _) =
                super::read_stops(&mut handler, &mut comments, &mut equipments).unwrap();
            let equipments_collection =
                CollectionWithId::new(equipments.into_equipments()).unwrap();
            assert_eq!(2, stop_areas.len());
            assert_eq!(2, stop_points.len());
            assert_eq!(2, equipments_collection.len());

            let mut stop_point_equipment_ids: Vec<Option<String>> = stop_points
                .iter()
                .map(|(_, stop_point)| stop_point.equipment_id.clone())
                .collect();
            stop_point_equipment_ids.sort();
            assert_eq!(vec![None, Some("0".to_string())], stop_point_equipment_ids);

            assert_eq!(
                vec![&None, &Some("1".to_string())],
                extract(|sa| &sa.equipment_id, &stop_areas)
            );
            use objects::Availability::*;
            assert_eq!(
                vec![
                    Equipment {
                        id: "0".to_string(),
                        wheelchair_boarding: Available,
                        sheltered: InformationNotAvailable,
                        elevator: InformationNotAvailable,
                        escalator: InformationNotAvailable,
                        bike_accepted: InformationNotAvailable,
                        bike_depot: InformationNotAvailable,
                        visual_announcement: InformationNotAvailable,
                        audible_announcement: InformationNotAvailable,
                        appropriate_escort: InformationNotAvailable,
                        appropriate_signage: InformationNotAvailable,
                    },
                    Equipment {
                        id: "1".to_string(),
                        wheelchair_boarding: NotAvailable,
                        sheltered: InformationNotAvailable,
                        elevator: InformationNotAvailable,
                        escalator: InformationNotAvailable,
                        bike_accepted: InformationNotAvailable,
                        bike_depot: InformationNotAvailable,
                        visual_announcement: InformationNotAvailable,
                        audible_announcement: InformationNotAvailable,
                        appropriate_escort: InformationNotAvailable,
                        appropriate_signage: InformationNotAvailable,
                    },
                ],
                equipments_collection.into_vec()
            );
        });
    }

    #[test]
    fn stops_do_not_generate_duplicate_equipments() {
        let stops_content = "stop_id,stop_name,stop_lat,stop_lon,location_type,parent_station,wheelchair_boarding\n\
                             sp:01,my stop point name 1,0.1,1.2,0,,1\n\
                             sp:02,my stop point name 2,0.2,1.5,0,,1";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "stops.txt", stops_content);

            let mut comments: CollectionWithId<Comment> = CollectionWithId::default();
            let mut equipments = EquipmentList::default();
            let (_, stop_points, _) =
                super::read_stops(&mut handler, &mut comments, &mut equipments).unwrap();
            let equipments_collection =
                CollectionWithId::new(equipments.into_equipments()).unwrap();
            assert_eq!(2, stop_points.len());
            assert_eq!(1, equipments_collection.len());

            let mut stop_point_equipment_ids: Vec<Option<String>> = stop_points
                .iter()
                .map(|(_, stop_point)| stop_point.equipment_id.clone())
                .collect();
            stop_point_equipment_ids.sort();
            assert_eq!(
                vec![Some("0".to_string()), Some("0".to_string())],
                stop_point_equipment_ids
            );

            use objects::Availability::*;
            assert_eq!(
                vec![Equipment {
                    id: "0".to_string(),
                    wheelchair_boarding: Available,
                    sheltered: InformationNotAvailable,
                    elevator: InformationNotAvailable,
                    escalator: InformationNotAvailable,
                    bike_accepted: InformationNotAvailable,
                    bike_depot: InformationNotAvailable,
                    visual_announcement: InformationNotAvailable,
                    audible_announcement: InformationNotAvailable,
                    appropriate_escort: InformationNotAvailable,
                    appropriate_signage: InformationNotAvailable,
                }],
                equipments_collection.into_vec()
            );
        });
    }

    #[test]
    fn gtfs_stop_times_estimated() {
        let routes_content = "route_id,agency_id,route_short_name,route_long_name,route_type,route_color,route_text_color\n\
                              route_1,agency_1,1,My line 1,3,8F7A32,FFFFFF";

        let stops_content =
            "stop_id,stop_name,stop_desc,stop_lat,stop_lon,location_type,parent_station\n\
             sp:01,my stop point name 1,my first desc,0.1,1.2,0,\n\
             sp:02,my stop point name 2,,0.2,1.5,0,\n\
             sp:03,my stop point name 2,,0.2,1.5,0,";

        let trips_content =
            "trip_id,route_id,direction_id,service_id,wheelchair_accessible,bikes_allowed\n\
             1,route_1,0,service_1,,";

        let stop_times_content = "trip_id,arrival_time,departure_time,stop_id,stop_sequence,stop_headsign,pickup_type,drop_off_type,shape_dist_traveled,timepoint\n\
                                  1,06:00:00,06:00:00,sp:01,1,over there,,,,0\n\
                                  1,06:06:27,06:06:27,sp:02,2,,2,1,,1\n\
                                  1,06:06:27,06:06:27,sp:03,3,,2,1,,";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);
            create_file_with_content(path, "stop_times.txt", stop_times_content);
            create_file_with_content(path, "stops.txt", stops_content);

            let mut collections = Collections::default();
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();

            let mut comments: CollectionWithId<Comment> = CollectionWithId::default();
            let mut equipments = EquipmentList::default();
            let (_, stop_points, _) =
                super::read_stops(&mut handler, &mut comments, &mut equipments).unwrap();
            collections.stop_points = stop_points;

            super::read_routes(&mut handler, &mut collections, false).unwrap();
            super::manage_stop_times(&mut collections, &mut handler, false, None).unwrap();

            assert_eq!(
                vec![
                    StopTime {
                        stop_point_idx: collections.stop_points.get_idx("sp:01").unwrap(),
                        sequence: 1,
                        arrival_time: Time::new(6, 0, 0),
                        departure_time: Time::new(6, 0, 0),
                        boarding_duration: 0,
                        alighting_duration: 0,
                        pickup_type: 0,
                        drop_off_type: 0,
                        datetime_estimated: true,
                        local_zone_id: None,
                        precision: Some(StopTimePrecision::Approximate),
                    },
                    StopTime {
                        stop_point_idx: collections.stop_points.get_idx("sp:02").unwrap(),
                        sequence: 2,
                        arrival_time: Time::new(6, 6, 27),
                        departure_time: Time::new(6, 6, 27),
                        boarding_duration: 0,
                        alighting_duration: 0,
                        pickup_type: 2,
                        drop_off_type: 1,
                        datetime_estimated: false,
                        local_zone_id: None,
                        precision: Some(StopTimePrecision::Exact),
                    },
                    StopTime {
                        stop_point_idx: collections.stop_points.get_idx("sp:03").unwrap(),
                        sequence: 3,
                        arrival_time: Time::new(6, 6, 27),
                        departure_time: Time::new(6, 6, 27),
                        boarding_duration: 0,
                        alighting_duration: 0,
                        pickup_type: 2,
                        drop_off_type: 1,
                        datetime_estimated: false,
                        local_zone_id: None,
                        precision: Some(StopTimePrecision::Exact),
                    },
                ],
                collections.vehicle_journeys.into_vec()[0].stop_times
            );
        });
    }

    #[test]
    fn gtfs_stop_times() {
        let routes_content = "route_id,agency_id,route_short_name,route_long_name,route_type,route_color,route_text_color\n\
                              route_1,agency_1,1,My line 1,3,8F7A32,FFFFFF";

        let stops_content =
            "stop_id,stop_name,stop_desc,stop_lat,stop_lon,location_type,parent_station\n\
             sp:01,my stop point name 1,my first desc,0.1,1.2,0,\n\
             sp:02,my stop point name 2,,0.2,1.5,0,";

        let trips_content =
            "trip_id,route_id,direction_id,service_id,wheelchair_accessible,bikes_allowed\n\
             1,route_1,0,service_1,,";

        let stop_times_content = "trip_id,arrival_time,departure_time,stop_id,stop_sequence,stop_headsign,pickup_type,drop_off_type,shape_dist_traveled\n\
                                  1,06:00:00,06:00:00,sp:01,1,over there,,,\n\
                                  1,06:06:27,06:06:27,sp:02,2,,2,1,";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);
            create_file_with_content(path, "stop_times.txt", stop_times_content);
            create_file_with_content(path, "stops.txt", stops_content);

            let mut collections = Collections::default();
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();

            let mut comments: CollectionWithId<Comment> = CollectionWithId::default();
            let mut equipments = EquipmentList::default();
            let (_, stop_points, _) =
                super::read_stops(&mut handler, &mut comments, &mut equipments).unwrap();
            collections.stop_points = stop_points;

            super::read_routes(&mut handler, &mut collections, false).unwrap();
            super::manage_stop_times(&mut collections, &mut handler, false, None).unwrap();

            assert_eq!(
                vec![
                    StopTime {
                        stop_point_idx: collections.stop_points.get_idx("sp:01").unwrap(),
                        sequence: 1,
                        arrival_time: Time::new(6, 0, 0),
                        departure_time: Time::new(6, 0, 0),
                        boarding_duration: 0,
                        alighting_duration: 0,
                        pickup_type: 0,
                        drop_off_type: 0,
                        datetime_estimated: false,
                        local_zone_id: None,
                        precision: Some(StopTimePrecision::Exact),
                    },
                    StopTime {
                        stop_point_idx: collections.stop_points.get_idx("sp:02").unwrap(),
                        sequence: 2,
                        arrival_time: Time::new(6, 6, 27),
                        departure_time: Time::new(6, 6, 27),
                        boarding_duration: 0,
                        alighting_duration: 0,
                        pickup_type: 2,
                        drop_off_type: 1,
                        datetime_estimated: false,
                        local_zone_id: None,
                        precision: Some(StopTimePrecision::Exact),
                    },
                ],
                collections.vehicle_journeys.into_vec()[0].stop_times
            );
            let headsigns: Vec<String> =
                collections.stop_time_headsigns.values().cloned().collect();
            assert_eq!(vec!["over there".to_string()], headsigns);
        });
    }

    #[test]
    fn read_tranfers() {
        let stops_content = "stop_id,stop_name,stop_lat,stop_lon,location_type,parent_station,wheelchair_boarding\n\
                             sp:01,my stop point name 1,48.857332,2.346331,0,,1\n\
                             sp:02,my stop point name 2,48.858195,2.347448,0,,1\n\
                             sp:03,my stop point name 3,48.859031,2.346958,0,,1";

        let transfers_content = "from_stop_id,to_stop_id,transfer_type,min_transfer_time\n\
                                 sp:01,sp:01,1,\n\
                                 sp:01,sp:02,0,\n\
                                 sp:01,sp:03,2,60\n\
                                 sp:02,sp:01,0,\n\
                                 sp:02,sp:02,1,\n\
                                 sp:02,sp:03,3,\n\
                                 sp:03,sp:01,0,\n\
                                 sp:03,sp:02,2,\n\
                                 sp:03,sp:03,0,";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "stops.txt", stops_content);
            create_file_with_content(path, "transfers.txt", transfers_content);

            let mut comments: CollectionWithId<Comment> = CollectionWithId::default();
            let mut equipments = EquipmentList::default();
            let (stop_areas, stop_points, _) =
                super::read_stops(&mut handler, &mut comments, &mut equipments).unwrap();

            let transfers = super::read_transfers(&mut handler, &stop_points, &stop_areas).unwrap();
            assert_eq!(
                vec![
                    &Transfer {
                        from_stop_id: "sp:01".to_string(),
                        to_stop_id: "sp:01".to_string(),
                        min_transfer_time: Some(0),
                        real_min_transfer_time: Some(0),
                        equipment_id: None,
                    },
                    &Transfer {
                        from_stop_id: "sp:01".to_string(),
                        to_stop_id: "sp:02".to_string(),
                        min_transfer_time: Some(160),
                        real_min_transfer_time: Some(280),
                        equipment_id: None,
                    },
                    &Transfer {
                        from_stop_id: "sp:01".to_string(),
                        to_stop_id: "sp:03".to_string(),
                        min_transfer_time: Some(60),
                        real_min_transfer_time: Some(60),
                        equipment_id: None,
                    },
                    &Transfer {
                        from_stop_id: "sp:02".to_string(),
                        to_stop_id: "sp:01".to_string(),
                        min_transfer_time: Some(160),
                        real_min_transfer_time: Some(280),
                        equipment_id: None,
                    },
                    &Transfer {
                        from_stop_id: "sp:02".to_string(),
                        to_stop_id: "sp:02".to_string(),
                        min_transfer_time: Some(0),
                        real_min_transfer_time: Some(0),
                        equipment_id: None,
                    },
                    &Transfer {
                        from_stop_id: "sp:02".to_string(),
                        to_stop_id: "sp:03".to_string(),
                        min_transfer_time: Some(86400),
                        real_min_transfer_time: Some(86400),
                        equipment_id: None,
                    },
                    &Transfer {
                        from_stop_id: "sp:03".to_string(),
                        to_stop_id: "sp:01".to_string(),
                        min_transfer_time: Some(247),
                        real_min_transfer_time: Some(367),
                        equipment_id: None,
                    },
                    &Transfer {
                        from_stop_id: "sp:03".to_string(),
                        to_stop_id: "sp:02".to_string(),
                        min_transfer_time: None,
                        real_min_transfer_time: None,
                        equipment_id: None,
                    },
                    &Transfer {
                        from_stop_id: "sp:03".to_string(),
                        to_stop_id: "sp:03".to_string(),
                        min_transfer_time: Some(0),
                        real_min_transfer_time: Some(120),
                        equipment_id: None,
                    },
                ],
                transfers.values().collect::<Vec<_>>()
            );
        });
    }

    #[test]
    fn gtfs_with_calendars_and_no_calendar_dates() {
        let content = "service_id,monday,tuesday,wednesday,thursday,friday,saturday,sunday,start_date,end_date\n\
                       1,0,0,0,0,0,1,1,20180501,20180508\n\
                       2,1,0,0,0,0,0,0,20180502,20180506";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "calendar.txt", content);

            let mut collections = Collections::default();
            calendars::manage_calendars(&mut handler, &mut collections).unwrap();

            let mut dates = BTreeSet::new();
            dates.insert(chrono::NaiveDate::from_ymd(2018, 5, 5));
            dates.insert(chrono::NaiveDate::from_ymd(2018, 5, 6));
            assert_eq!(
                vec![Calendar {
                    id: "1".to_string(),
                    dates,
                },],
                collections.calendars.into_vec()
            );
        });
    }

    #[test]
    fn gtfs_with_calendars_dates_and_no_calendar() {
        let content = "service_id,date,exception_type\n\
                       1,20180212,1\n\
                       1,20180211,2\n\
                       2,20180211,2";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "calendar_dates.txt", content);

            let mut collections = Collections::default();
            calendars::manage_calendars(&mut handler, &mut collections).unwrap();

            let mut dates = BTreeSet::new();
            dates.insert(chrono::NaiveDate::from_ymd(2018, 2, 12));
            assert_eq!(
                vec![Calendar {
                    id: "1".to_string(),
                    dates,
                }],
                collections.calendars.into_vec()
            );
        });
    }

    #[test]
    fn gtfs_with_calendars_and_calendar_dates() {
        let calendars_content = "service_id,monday,tuesday,wednesday,thursday,friday,saturday,sunday,start_date,end_date\n\
                                 1,0,0,0,0,0,1,1,20180501,20180508\n\
                                 2,0,0,0,0,0,0,1,20180502,20180506";

        let calendar_dates_content = "service_id,date,exception_type\n\
                                      1,20180507,1\n\
                                      1,20180505,2\n\
                                      2,20180506,2";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "calendar.txt", calendars_content);

            create_file_with_content(path, "calendar_dates.txt", calendar_dates_content);

            let mut collections = Collections::default();
            calendars::manage_calendars(&mut handler, &mut collections).unwrap();

            let mut dates = BTreeSet::new();
            dates.insert(chrono::NaiveDate::from_ymd(2018, 5, 6));
            dates.insert(chrono::NaiveDate::from_ymd(2018, 5, 7));
            assert_eq!(
                vec![
                    Calendar {
                        id: "1".to_string(),
                        dates,
                    },
                    Calendar {
                        id: "2".to_string(),
                        dates: BTreeSet::new(),
                    },
                ],
                collections.calendars.into_vec()
            );
        });
    }

    #[test]
    #[should_panic(expected = "calendar_dates.txt or calendar.txt not found")]
    fn gtfs_without_calendar_dates_or_calendar() {
        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            let mut collections = Collections::default();
            calendars::manage_calendars(&mut handler, &mut collections).unwrap();
        });
    }

    #[test]
    fn read_shapes() {
        let shapes_content = "shape_id,shape_pt_lat,shape_pt_lon,shape_pt_sequence\n\
                              1,4.4,3.3,2\n\
                              1,2.2,1.1,1\n\
                              2,6.6,5.5,1\n\
                              2,,7.7,2\n\
                              2,8.8,,3\n\
                              2,,,4\n\
                              2,,,5\n\
                              3,,,1\n\
                              3,,,2";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "shapes.txt", shapes_content);

            let mut collections = Collections::default();
            super::manage_shapes(&mut collections, &mut handler).unwrap();
            let mut geometries = collections.geometries.into_vec();
            geometries.sort_unstable_by_key(|s| s.id.clone());

            assert_eq!(
                vec![
                    Geometry {
                        id: "1".to_string(),
                        geometry: line_string![(x: 1.1, y: 2.2), (x: 3.3, y: 4.4)].into(),
                    },
                    Geometry {
                        id: "2".to_string(),
                        geometry: line_string![(x: 5.5, y: 6.6)].into(),
                    },
                ],
                geometries
            );
        });
    }

    #[test]
    fn read_shapes_with_no_shapes_file() {
        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            let mut collections = Collections::default();
            super::manage_shapes(&mut collections, &mut handler).unwrap();
            let geometries = collections.geometries.into_vec();
            assert_eq!(Vec::<Geometry>::new(), geometries);
        });
    }

    #[test]
    fn deduplicate_funicular_physical_mode() {
        let routes_content = "route_id,agency_id,route_short_name,route_long_name,route_desc,route_type,route_url,route_color,route_text_color\n\
                                 route:1,agency:1,S1,S 1,,5,,ffea00,000000\n\
                                 route:2,agency:1,L2,L 2,,6,,ffea00,000000\n\
                                 route:3,agency:1,L3,L 3,,2,,ffea00,000000\n\
                                 route:4,agency:2,57,57,,7,,ffea00,000000";
        let trips_content = "route_id,service_id,trip_id,trip_headsign,direction_id,shape_id\n\
                             route:1,service:1,trip:1,pouet,0,\n\
                             route:2,service:1,trip:2,pouet,0,\n\
                             route:3,service:1,trip:3,pouet,0,\n\
                             route:4,service:1,trip:4,pouet,0,";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);

            let mut collections = Collections::default();
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();

            super::read_routes(&mut handler, &mut collections, false).unwrap();
            // physical mode file should contain only three modes
            // (5,7 => funicular; 2 => train; 6 => suspended cable car)
            assert_eq!(4, collections.lines.len());
            assert_eq!(4, collections.commercial_modes.len());
            assert_eq!(
                vec!["Funicular", "SuspendedCableCar", "Train"],
                extract_ids(&collections.physical_modes)
            );
        });
    }

    #[test]
    fn location_type_default_value() {
        let stops_content = "stop_id,stop_name,stop_lat,stop_lon,location_type\n\
                             stop:1,Tornio pouet,65.843294,24.145138,";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "stops.txt", stops_content);
            let mut equipments = EquipmentList::default();
            let mut comments: CollectionWithId<Comment> = CollectionWithId::default();
            let (stop_areas, stop_points, _) =
                super::read_stops(&mut handler, &mut comments, &mut equipments).unwrap();
            assert_eq!(1, stop_points.len());
            assert_eq!(1, stop_areas.len());
            let stop_area = stop_areas.iter().next().unwrap().1;
            assert_eq!("Navitia:stop:1", stop_area.id);
            let stop_point = stop_points.iter().next().unwrap().1;
            assert_eq!("stop:1", stop_point.id);
        });
    }

    #[test]
    fn location_with_space_proof() {
        let stops_content = "stop_id,stop_name,stop_lat,stop_lon,location_type\n\
                             stop:1,plop, 65.444,24.156 ,0\n\
                             stop:2,plop 2, 66.666 , 26.123,0\n\
                             stop:3,invalid loc, ,25.558,0";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "stops.txt", stops_content);
            let mut equipments = EquipmentList::default();
            let mut comments: CollectionWithId<Comment> = CollectionWithId::default();
            let (_, stop_points, _) =
                super::read_stops(&mut handler, &mut comments, &mut equipments).unwrap();
            assert_eq!(3, stop_points.len());
            let longitudes: Vec<f64> = stop_points
                .values()
                .map(|sp| &sp.coord.lon)
                .cloned()
                .collect();
            assert_eq!(vec![24.156, 26.123, 25.558], longitudes);
            let latitudes: Vec<f64> = stop_points
                .values()
                .map(|sp| &sp.coord.lat)
                .cloned()
                .collect();
            assert_eq!(vec![65.444, 66.666, 0.00], latitudes);
        });
    }

    #[test]
    fn gtfs_undefined_stop_times() {
        let routes_content = "route_id,agency_id,route_short_name,route_long_name,route_type,route_color,route_text_color\n\
                              route_1,agency_1,1,My line 1,3,8F7A32,FFFFFF";

        let stops_content = r#"stop_id,stop_name,stop_desc,stop_lat,stop_lon,location_type,parent_station
             sp:01,my stop point name 1,my first desc,0.1,1.2,0,
             sp:02,my stop point name 2,my first desc,0.1,1.2,0,
             sp:03,my stop point name 3,my first desc,0.1,1.2,0,
             sp:04,my stop point name 4,my first desc,0.1,1.2,0,
             sp:05,my stop point name 5,my first desc,0.1,1.2,0,
             sp:06,my stop point name 6,my first desc,0.1,1.2,0,
             sp:07,my stop point name 7,my first desc,0.1,1.2,0,
             sp:08,my stop point name 8,my first desc,0.1,1.2,0,"#;

        let trips_content =
            "trip_id,route_id,direction_id,service_id,wheelchair_accessible,bikes_allowed\n\
             1,route_1,0,service_1,,";

        let stop_times_content = "trip_id,arrival_time,departure_time,stop_id,stop_sequence,stop_headsign,pickup_type,drop_off_type,shape_dist_traveled\n\
                                  1,06:00:00,06:00:00,sp:01,1,,,,\n\
                                  1,07:00:00,07:00:00,sp:02,2,,,,\n\
                                  1,,,sp:03,3,,,,\n\
                                  1,,,sp:04,4,,,,\n\
                                  1,10:00:00,,sp:05,5,,,,\n\
                                  1,,,sp:06,6,,,,\n\
                                  1,,12:00:00,sp:07,7,,,,\n\
                                  1,13:00:00,13:00:00,sp:08,8,,,,\n\
                                  ";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);
            create_file_with_content(path, "stop_times.txt", stop_times_content);
            create_file_with_content(path, "stops.txt", stops_content);

            let mut collections = Collections::default();
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();

            let mut comments: CollectionWithId<Comment> = CollectionWithId::default();
            let mut equipments = EquipmentList::default();
            let (_, stop_points, _) =
                super::read_stops(&mut handler, &mut comments, &mut equipments).unwrap();
            collections.stop_points = stop_points;

            super::read_routes(&mut handler, &mut collections, false).unwrap();
            super::manage_stop_times(&mut collections, &mut handler, false, None).unwrap();

            assert_eq!(
                vec![
                    (Time::new(6, 0, 0), Time::new(6, 0, 0)),
                    (Time::new(7, 0, 0), Time::new(7, 0, 0)),
                    (Time::new(8, 0, 0), Time::new(8, 0, 0)),
                    (Time::new(9, 0, 0), Time::new(9, 0, 0)),
                    (Time::new(10, 0, 0), Time::new(10, 0, 0)),
                    (Time::new(11, 0, 0), Time::new(11, 0, 0)),
                    (Time::new(12, 0, 0), Time::new(12, 0, 0)),
                    (Time::new(13, 0, 0), Time::new(13, 0, 0)),
                ],
                collections.vehicle_journeys.into_vec()[0]
                    .stop_times
                    .iter()
                    .map(|st| (st.arrival_time, st.departure_time))
                    .collect::<Vec<_>>()
            );
        });
    }

    #[test]
    fn gtfs_invalid_undefined_stop_times() {
        let routes_content = "route_id,agency_id,route_short_name,route_long_name,route_type,route_color,route_text_color\n\
                              route_1,agency_1,1,My line 1,3,8F7A32,FFFFFF";

        let stops_content = r#"stop_id,stop_name,stop_desc,stop_lat,stop_lon,location_type,parent_station
             sp:01,my stop point name 1,my first desc,0.1,1.2,0,
             sp:02,my stop point name 2,my first desc,0.1,1.2,0,
             sp:03,my stop point name 3,my first desc,0.1,1.2,0,
             sp:04,my stop point name 4,my first desc,0.1,1.2,0,
             sp:05,my stop point name 5,my first desc,0.1,1.2,0,
             sp:06,my stop point name 6,my first desc,0.1,1.2,0,
             sp:07,my stop point name 7,my first desc,0.1,1.2,0,
             sp:08,my stop point name 8,my first desc,0.1,1.2,0,"#;

        let trips_content =
            "trip_id,route_id,direction_id,service_id,wheelchair_accessible,bikes_allowed\n\
             1,route_1,0,service_1,,";

        let stop_times_content = "trip_id,arrival_time,departure_time,stop_id,stop_sequence,stop_headsign,pickup_type,drop_off_type,shape_dist_traveled\n\
                                  1,,,sp:01,1,,,,\n\
                                  1,07:00:00,07:00:00,sp:02,2,,,,\n\
                                  1,,,sp:03,3,,,,\n\
                                  1,,,sp:04,4,,,,\n\
                                  1,10:00:00,10:00:00,sp:05,5,,,,\n\
                                  1,,,sp:06,6,,,,\n\
                                  1,12:00:00,12:00:00,sp:07,7,,,,\n\
                                  1,13:00:00,13:00:00,sp:08,8,,,,\n\
                                  ";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);
            create_file_with_content(path, "stop_times.txt", stop_times_content);
            create_file_with_content(path, "stops.txt", stops_content);

            let mut collections = Collections::default();
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();

            let mut comments: CollectionWithId<Comment> = CollectionWithId::default();
            let mut equipments = EquipmentList::default();
            let (_, stop_points, _) =
                super::read_stops(&mut handler, &mut comments, &mut equipments).unwrap();
            collections.stop_points = stop_points;

            super::read_routes(&mut handler, &mut collections, false).unwrap();
            let val = super::manage_stop_times(&mut collections, &mut handler, false, None);

            // the first stop time of the vj has no departure/arrival, it's an error
            let err = val.unwrap_err();
            assert_eq!( "the first stop time of the vj '1' has no departure/arrival, the stop_times.txt file is not valid",format!("{}", err));
        });
    }
    #[test]
    fn stop_location_on_stops() {
        let stops_content =
            "stop_id,stop_code,stop_name,stop_lat,stop_lon,location_type,parent_station\n\
             stoppoint_id,1234,my stop name,0.1,1.2,0,stop_area_id\n\
             stoparea_id,5678,stop area name,0.1,1.2,1,\n\
             entrance_id,,entrance name,0.1,1.2,2,stop_area_id\n\
             node_id,,node name,0.1,1.2,3,stop_area_id\n\
             boarding_id,,boarding name,0.1,1.2,4,stoppoint_id";
        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "stops.txt", stops_content);
            let mut equipments = EquipmentList::default();
            let mut comments: CollectionWithId<Comment> = CollectionWithId::default();
            let (_, _, stop_locations) =
                super::read_stops(&mut handler, &mut comments, &mut equipments).unwrap();
            let stop_entrance = stop_locations
                .values()
                .filter(|sl| sl.stop_type == StopType::StopEntrance);
            assert_eq!(1, stop_entrance.count());
            let stop_node = stop_locations
                .values()
                .filter(|sl| sl.stop_type == StopType::GenericNode);
            assert_eq!(1, stop_node.count());
            let stop_boarding = stop_locations
                .values()
                .filter(|sl| sl.stop_type == StopType::GenericNode);
            assert_eq!(1, stop_boarding.count());
        });
    }
    #[test]
    fn filter_pathway() {
        let stops_content =
            "stop_id,stop_code,stop_name,stop_lat,stop_lon,location_type,parent_station,level_id\n\
             stoppoint_id,1234,my stop name,0.1,1.2,0,stop_area_id,2\n\
             stoparea_id,5678,stop area name,0.1,1.2,1,,\n\
             entrance_id,,entrance name,0.1,1.2,2,stop_area_id,1\n\
             node_id,,node name,0.1,1.2,3,stop_area_id,2\n\
             boarding_id,,boarding name,0.1,1.2,4,stoppoint_id,";
        let pathway_content = "pathway_id,from_stop_id,to_stop_id,pathway_mode,is_bidirectional\n\
                               1;stoppoint_id,stoparea_id,8,0\n\
                               2,stoppoint_id,stoparea_id,1,3\n\
                               3,stoppoint_id,stoparea_id_0,2,0\n\
                               4,stoppoint_id,stoparea_id,1,0
                               5,node_id,boarding_id,1,0";
        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "stops.txt", stops_content);
            create_file_with_content(path, "pathways.txt", pathway_content);
            let mut collections = Collections::default();
            let mut equipments = EquipmentList::default();
            let (_, stop_points, stop_locations) =
                super::read_stops(&mut handler, &mut collections.comments, &mut equipments)
                    .unwrap();
            collections.stop_points = stop_points;
            collections.stop_locations = stop_locations;

            super::manage_pathways(&mut collections, &mut handler).unwrap();
            assert_eq!(1, collections.pathways.len());
        })
    }
    #[test]
    fn read_levels() {
        let stops_content =
            "stop_id,stop_code,stop_name,stop_lat,stop_lon,location_type,parent_station,level_id\n\
             stoppoint_id,1234,my stop name,0.1,1.2,0,stop_area_id,2\n\
             stoparea_id,5678,stop area name,0.1,1.2,1,,\n\
             entrance_id,,entrance name,0.1,1.2,2,stop_area_id,1\n\
             node_id,,node name,0.1,1.2,3,stop_area_id,2\n\
             boarding_id,,boarding name,0.1,1.2,4,stoppoint_id,";
        let level_content = "level_id,level_index\n\
                             1,0\n\
                             2,2\n\
                             3,1\n\
                             4,4";
        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "stops.txt", stops_content);
            create_file_with_content(path, "levels.txt", level_content);
            let levels: CollectionWithId<Level> =
                read_opt_collection(&mut handler, "levels.txt").unwrap();
            assert_eq!(4, levels.len());
        })
    }
    #[test]
    fn gtfs_stop_times_precision() {
        let routes_content = "route_id,agency_id,route_short_name,route_long_name,route_type,route_color,route_text_color\n\
                              route_1,agency_1,1,My line 1,3,8F7A32,FFFFFF";

        let stops_content =
            "stop_id,stop_name,stop_desc,stop_lat,stop_lon,location_type,parent_station\n\
             sp:01,my stop point name 1,my first desc,0.1,1.2,0,\n\
             sp:02,my stop point name 2,,0.2,1.5,0,\n\
             sp:03,my stop point name 2,,0.2,1.5,0,";

        let trips_content =
            "trip_id,route_id,direction_id,service_id,wheelchair_accessible,bikes_allowed\n\
             1,route_1,0,service_1,,";

        let stop_times_content = "trip_id,arrival_time,departure_time,stop_id,stop_sequence,stop_headsign,pickup_type,drop_off_type,shape_dist_traveled,timepoint\n\
                                  1,06:00:00,06:00:00,sp:01,1,over there,,,,0\n\
                                  1,06:06:27,06:06:27,sp:02,2,,2,1,,1\n\
                                  1,06:06:27,06:06:27,sp:03,3,,2,1,,";

        test_in_tmp_dir(|path| {
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);
            create_file_with_content(path, "stop_times.txt", stop_times_content);
            create_file_with_content(path, "stops.txt", stops_content);

            let mut collections = Collections::default();
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();

            let mut comments: CollectionWithId<Comment> = CollectionWithId::default();
            let mut equipments = EquipmentList::default();
            let (_, stop_points, _) =
                super::read_stops(&mut handler, &mut comments, &mut equipments).unwrap();
            collections.stop_points = stop_points;

            super::read_routes(&mut handler, &mut collections, false).unwrap();
            super::manage_stop_times(&mut collections, &mut handler, true, None).unwrap();

            assert_eq!(
                vec![
                    StopTime {
                        stop_point_idx: collections.stop_points.get_idx("sp:01").unwrap(),
                        sequence: 1,
                        arrival_time: Time::new(6, 0, 0),
                        departure_time: Time::new(6, 0, 0),
                        boarding_duration: 0,
                        alighting_duration: 0,
                        pickup_type: 0,
                        drop_off_type: 0,
                        datetime_estimated: true,
                        local_zone_id: None,
                        precision: Some(StopTimePrecision::Estimated),
                    },
                    StopTime {
                        stop_point_idx: collections.stop_points.get_idx("sp:02").unwrap(),
                        sequence: 2,
                        arrival_time: Time::new(6, 6, 27),
                        departure_time: Time::new(6, 6, 27),
                        boarding_duration: 0,
                        alighting_duration: 0,
                        pickup_type: 2,
                        drop_off_type: 1,
                        datetime_estimated: false,
                        local_zone_id: None,
                        precision: Some(StopTimePrecision::Exact),
                    },
                    StopTime {
                        stop_point_idx: collections.stop_points.get_idx("sp:03").unwrap(),
                        sequence: 3,
                        arrival_time: Time::new(6, 6, 27),
                        departure_time: Time::new(6, 6, 27),
                        boarding_duration: 0,
                        alighting_duration: 0,
                        pickup_type: 2,
                        drop_off_type: 1,
                        datetime_estimated: false,
                        local_zone_id: None,
                        precision: Some(StopTimePrecision::Exact),
                    },
                ],
                collections.vehicle_journeys.into_vec()[0].stop_times
            );
        });
    }

    mod read_gtfs_routes {
        use super::*;
        use crate::{
            model::Collections,
            read_utils::{self, PathFileHandler},
        };
        use pretty_assertions::assert_eq;
        use std::path;

        fn get_collection(path: &path::Path, read_as_line: bool) -> Collections {
            let agency_content = "agency_id,agency_name,agency_url,agency_timezone\n\
            id_agency,My agency,http://my-agency_url.com,Europe/London";

            let routes_content = "route_id,agency_id,route_short_name,route_long_name,route_type,route_color,route_text_color\n\
                        route_1,agency_1,1,My line 1A,3,8F7A32,FFFFFF\n\
                        route_2,agency_1,1,My line 1B,3,8F7A32,FFFFFF\n\
                        route_4,agency_2,1,My line 1B,3,8F7A32,FFFFFF\n\
                        route_3,agency_2,1,My line 1B,3,8F7A32,FFFFFF\n\
                        route_5,,1,My line 1C,3,8F7A32,FFFFFF";

            let trips_content =
                "trip_id,route_id,direction_id,service_id,wheelchair_accessible,bikes_allowed\n\
            1,route_1,0,service_1,,\n\
            2,route_2,0,service_1,,\n\
            3,route_3,0,service_2,,\n\
            4,route_4,0,service_2,,\n\
            5,route_5,0,service_2,,\n\
            6,route_5,1,service_2,,";
            let mut handler = PathFileHandler::new(path.to_path_buf());
            create_file_with_content(path, "agency.txt", agency_content);
            create_file_with_content(path, "routes.txt", routes_content);
            create_file_with_content(path, "trips.txt", trips_content);
            let mut collections = Collections::default();
            let (networks, _) = super::read_agency(&mut handler).unwrap();
            collections.networks = networks;
            let (contributor, dataset, _) = read_utils::read_config(None::<&str>).unwrap();
            collections.contributors = CollectionWithId::new(vec![contributor]).unwrap();
            collections.datasets = CollectionWithId::new(vec![dataset]).unwrap();
            super::read_routes(&mut handler, &mut collections, read_as_line).unwrap();
            collections
        }

        #[test]
        fn read_gtfs_routes_as_route() {
            test_in_tmp_dir(|path| {
                // read route as route
                let collections = get_collection(path, false);

                assert_eq!(3, collections.lines.len());
                assert_eq!(
                    vec!["agency_1", "agency_2", "id_agency"],
                    extract(|l| &l.network_id, &collections.lines)
                );
                assert_eq!(
                    vec!["route_1", "route_3", "route_5"],
                    extract_ids(&collections.lines)
                );
                assert_eq!(6, collections.routes.len());
                assert_eq!(
                    vec!["route_1", "route_1", "route_3", "route_3", "route_5", "route_5"],
                    extract(|r| &r.line_id, &collections.routes)
                );
                assert_eq!(
                    vec![
                        "route_1",
                        "route_2",
                        "route_3",
                        "route_4",
                        "route_5",
                        "route_5_R"
                    ],
                    extract(|r| &r.id, &collections.routes)
                );
            });
        }

        #[test]
        fn read_gtfs_routes_as_line() {
            test_in_tmp_dir(|path| {
                // read route as line
                let collections = get_collection(path, true);

                assert_eq!(5, collections.lines.len());
                assert_eq!(
                    vec!["agency_1", "agency_1", "agency_2", "agency_2", "id_agency"],
                    extract(|l| &l.network_id, &collections.lines)
                );
                assert_eq!(
                    vec!["route_1", "route_2", "route_3", "route_4", "route_5"],
                    extract_ids(&collections.lines)
                );
                assert_eq!(6, collections.routes.len());
                assert_eq!(
                    vec!["route_1", "route_2", "route_3", "route_4", "route_5", "route_5"],
                    extract(|r| &r.line_id, &collections.routes)
                );
                assert_eq!(
                    vec![
                        "route_1",
                        "route_2",
                        "route_3",
                        "route_4",
                        "route_5",
                        "route_5_R"
                    ],
                    extract(|r| &r.id, &collections.routes)
                );
            });
        }
    }
}
