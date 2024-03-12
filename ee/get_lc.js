// === TN Caribou: get land cover ---------------------------------------------
// Alec L. Robitaille



// Region ---------------------------------------------------------------------
var region =  ee.Geometry.Polygon(
        [[[-54.376, 48.327],
          [-54.376, 48.750],
          [-53.571, 48.750],
          [-53.571, 48.327]]], null, false);

Map.addLayer(region);



// Images ---------------------------------------------------------------------
var nalcms = ee.Image('USGS/NLCD_RELEASES/2020_REL/NALCMS');
  
var hermosilla_2019 = ee.ImageCollection("projects/sat-io/open-datasets/CA_FOREST_LC_VLCE2")
  .filterDate('2019-01-01', '2020-01-01')
  .first();


// Export ---------------------------------------------------------------------
var today = new Date().toJSON().slice(0, 10);
var folder = 'TN Caribou';

Export.image.toDrive({
  image: nalcms,
  description: today + '_NALCMS_30_m',
  region: region,
  scale: 30,
  folder: folder
});

Export.image.toDrive({
  image: hermosilla_2019,
  description: today + '_Hermosilla_2019_land_cover',
  region: region,
  scale: 30,
  folder: folder
});
