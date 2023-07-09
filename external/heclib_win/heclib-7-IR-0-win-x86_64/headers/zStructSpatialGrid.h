#ifndef zSTRUCT_SPATIAL_GRID_H
#define zSTRUCT_SPATIAL_GRID_H

#define HRAP_SRC_DEFINITION "PROJCS[\"Stereographic_CONUS_HRAP\",\
GEOGCS[\"GCS_Sphere_LFM\",DATUM[\"D_Sphere_LFM\",\
SPHEROID[\"Shpere_LFM\",6371200.0,0.0]],PRIMEM[\"Greenwich\",0.0],\
UNIT[\"Degree\",0.0174532925199433]],\
PROJECTION[\"Stereographic_North_Pole\"],\
PARAMETER[\"False_Easting\",1909762.5],PARAMETER[\"False_Northing\",7624762.5],\
PARAMETER[\"Central_Meridian\",-105.0],PARAMETER[\"Standard_Parallel_1\",60.0],\
UNIT[\"Meter\",1.0]]"

#define SHG_SRC_DEFINITION "PROJCS[\"USA_Contiguous_Albers_Equal_Area_Conic_USGS_version\",\
GEOGCS[\"GCS_North_American_1983\",DATUM[\"D_North_American_1983\",\
SPHEROID[\"GRS_1980\",6378137.0,298.257222101]],PRIMEM[\"Greenwich\",0.0],\
UNIT[\"Degree\",0.0174532925199433]],PROJECTION[\"Albers\"],\
PARAMETER[\"False_Easting\",0.0],PARAMETER[\"False_Northing\",0.0],\
PARAMETER[\"Central_Meridian\",-96.0],PARAMETER[\"Standard_Parallel_1\",29.5],\
PARAMETER[\"Standard_Parallel_2\",45.5],PARAMETER[\"Latitude_Of_Origin\",23.0],\
UNIT[\"Meter\",1.0]]"

//Parameters: utmZone, utmHemisphere, central_meridian, false_northing
//if (_utmHemisphere.equals("S")) falseNorthing = 10000000;
// centralMeridian = -183 + _utmZone * 6;
#define UTM_SRC_DEFINITION "PROJCS[\"UTM_ZONE_%s%s_WGS84\",\
GEOGCS[\"WGS_84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS84\",6378137,298.257223563]],\
PRIMEM[\"Greenwich\",0],UNIT[\"degree\",0.01745329251994328]],\
UNIT[\"Meter\",1.0],PROJECTION[\"Transverse_Mercator\"],\
PARAMETER[\"latitude_of_origin\",0],\
PARAMETER[\"central_meridian\",%s],\
PARAMETER[\"scale_factor\",0.9996],PARAMETER[\"false_easting\",500000],\
PARAMETER[\"false_northing\",%s],\
AXIS[\"Easting\",EAST],AXIS[\"Northing\",NORTH]]"

enum ProjectionDatum {
	UNDEFINED_PROJECTION_DATUM, NAD_27, NAD_83
};

enum CompressionMethod {
	UNDEFINED_COMPRESSION_METHOD = 0,
	NO_COMPRESSION = 1,
	ZLIB_COMPRESSION = 26,
};

enum StorageDataType {
	GRID_FLOAT = 0, // Currently this is only supported data type
	GRID_INT = 1, // 4 byte int
	GRID_DOUBLE = 2,
	GRID_LONG = 3 // 8 byte int
};

enum dataType {
	PER_AVER = 0, PER_CUM = 1, INST_VAL = 2, INST_CUM = 3, FREQ = 4, INVALID = 5
};

enum gridStructVersion {
	VERSION_100 = -100
};

/**
* The SpecifiedGridInfo class inherits from GridInfo, extending the grid metadata to support
* any coordinate system, and adding new metadata fields that HEC has determined would
* be useful since the original GridInfo was defined in 1994.
*
* The new data fields are:
*   _srsName (String) The name of the spatial reference system (SRS) for the grid.
*   _srsDefinition (String) A complete spatial reference identifier in a text
*         of arbitrary length.  For present use, we are populating this field
*         with a Spatial Reference Identifier in well-known text (WKT) format.
*   _xCoordOfGridCellZero (float) The x coordinate value, in the SRS, of the
*         lower left corner (not the center) of cell (0, 0).
*   _yCoordOfGridCellZero (float) The y coordinate value, in the SRS, of the
*         lower left corner (not the center) of cell (0, 0).
*   _nullValue (float) the value used in the grid's data array to represent a "no data"
*         cell.
*   _timeZoneID (String) a name for the time zone that applies to the data in the grid.
*   _timeZoneRawOffset (int) Offset in milliseconds from UTC.
*   _isTimeStamped (boolean) false for data that is time-invariant (e.g. grids of
*         stable parameters, like ground elevation).
*   _isTSElement (boolean) false for data that is time-invariant (e.g. grids of
*         stable parameters, like ground elevation).
*   _isInterval (boolean) false for instantaneous values, true for period data.
*
* In addition, the GridInfo version number is explicitly stored in the header of the
* DSS record, and the compression method defaults to ZLIB's deflate method
*
* @author Tom Evans November 2011
*
*/

// Spatial Grid internal header
#define INT_HEAD_grid_structVersion 0
#define INT_HEAD_grid_type 1
#define INT_HEAD_grid_version 2
#define INT_HEAD_grid_dataType 3
#define INT_HEAD_grid_lowerLeftCellX 4
#define INT_HEAD_grid_lowerLeftCellY 5
#define INT_HEAD_grid_numberOfCellsX 6
#define INT_HEAD_grid_numberOfCellsY 7
#define INT_HEAD_grid_compressionMethod 8
#define INT_HEAD_grid_sizeofCompressedElements 9
#define INT_HEAD_grid_numberOfRanges 10
#define INT_HEAD_grid_srsDefinitionType 11
#define INT_HEAD_grid_timeZoneRawOffset 12
#define INT_HEAD_grid_isInterval 13
#define INT_HEAD_grid_isTimeStamped 14
#define INT_HEAD_grid_storageDataType 15
#define INT_HEAD_grid_internalHeaderNumber 24

typedef struct {
	//  Private
	int structType;

	/*  Required  */
	char *pathname;

	int _structVersion; // In case we want to modify the gridstruct later
	int _type; // DSS Grid Type 

			   // Don't store but in the DSS 
			   //int       _infoSize; 
			   //int       _gridInfoSize; 

	int _version;

	//int       _verbose;
	//int       _startTime;
	//int       _endTime ;

	char* _dataUnits;
	int _dataType;
	char* _dataSource; // Needed for HRAP grids 
	int _lowerLeftCellX;
	int _lowerLeftCellY;
	int _numberOfCellsX;
	int _numberOfCellsY;
	float _cellSize;
	int _compressionMethod; //zlib for initial implementation
	int _sizeofCompressedElements;
	void* _compressionParameters;

	char* _srsName;
	// for now we're using WKT strings for the SRS definitions, but 
	// here's a placeholder for future options like proj4 or GML
	int _srsDefinitionType;
	char* _srsDefinition;
	float _xCoordOfGridCellZero;
	float _yCoordOfGridCellZero;
	float _nullValue;
	char* _timeZoneID;
	int _timeZoneRawOffset;
	int _isInterval; // Originally boolean 
	int _isTimeStamped; // Originally boolean
	int _numberOfRanges;


	//Actual Data
	int _storageDataType;
	void *_maxDataValue;
	void *_minDataValue;
	void *_meanDataValue;
	void *_rangeLimitTable;
	int *_numberEqualOrExceedingRangeLimit;
	void *_data;

} zStructSpatialGrid;

zStructSpatialGrid* zstructSpatialGridNew(const char* pathname);
/*int compress_zfp(float*, int, int, float, void **, int *, int);*/
int compress_zlib(void* array, int size, void **buffer);
int uncompress_zlib(const void* buffer, int size, void* data, int dataSize);
int zspatialGridRetrieve(long long *ifltab, zStructSpatialGrid *gdStruct, int boolRetrieveData);
int zspatialGridRetrieveVersion(long long *ifltab, const char *cpath, int* gridStructVersion);
int zspatialGridStore(long long *ifltab, zStructSpatialGrid *gdStruct);
void printGridStruct(long long *ifltab, int funtion_id, zStructSpatialGrid *gdStruct);

#endif // zSTRUCT_SPATIAL_GRID_H

