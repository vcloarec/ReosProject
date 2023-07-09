#ifndef zSTRUCT_SPATIAL_TIN_H
#define zSTRUCT_SPATIAL_TIN_H

typedef struct {


	//  Private
	int structType;

	/*  Required  */
	char *pathname;

	// Geospatial metadata
	char* SpatialReferenceSystem;
	int SRSType; // enum type 0 = WKT;
	char* SRSName;
	char* SRSUnits;

	//  metadata for data values
	char *units;
	char *type;
	char *timeZoneName; //  Time zone of the data (may or may not match location time zone)
	float minXCoordinate;
	float maxXCoordinate;
	float minYCoordinate;
	float maxYCoordinate;
	float minValue;
	float meanValue;
	float maxValue;

	// Start and end times, required for TIN container, should be derived
	// from the DSS path.

	// TIN is made up of points and connections
	int numberPoints;		//  Dimension of arrays
	int connectTableLen;		//  Overall size of the connection array
	int pointLabelLen;			//  Length of label array in bytes
	float slendernessRatio; // weeds out splintered triangles

	// These arrays have one value per point
	float *xCoordinate;
	float *yCoordinate;
	float *value;
	int *pointType; // enumerated type 0 = DCP; 1 = observer gage; 2 = grid cell center;
	int *numberConnections;
	int *connectTo;   //  A doubly dimensioned array
	char *pointLabel;

	//  Private - knowing which variables were allocated by the ztsNew functions,
	//  instead of the calling program
	char allocated[zSTRUCT_length];

} zStructSpatialTin;

#endif  // zSTRUCT_SPATIAL_TIN_H

