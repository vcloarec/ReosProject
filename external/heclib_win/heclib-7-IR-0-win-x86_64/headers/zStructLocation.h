#ifndef zSTRUCT_LOCATION_H
#define zSTRUCT_LOCATION_H

#include "zStructAllocation.h"


typedef struct {

	//  Private
	int structType;

	char *pathname;  //  Can be any pathname from the data set.  Will be use to form location path.

	/*  ____________ All are Optional __________________________   */

	//  Location Description parameters

	/*  Location Identifiers  */
	 /* Longitude, Easting or decimal degrees (negative for Western Hemisphere) */
    double xOrdinate;
     /* Latitude, Northing or decimal degrees */
    double yOrdinate;
     /* Elevation */
    double zOrdinate;

    /**
	 * coordinateSystem <p>
	 *  <li>0 - no coordinates set </li>
	 *  <li>1 - Lat/long</li>
	 *  <li>2 - State Plane, FIPS </li>
	 *  <li>3 - State Plane, ADS</li>
	 *  <li>4 - UTM</li>
	 *  <li>5 - Local (other)</li>
	 */
    int coordinateSystem;
	 /* coordinateID = UTM zone #, or FIPS SPCS # ADS SPCS #  */
    int coordinateID;
    /**
	 * Horizontal Units can be one of the following:
	 * <li>0 - unspecified</li>
	 * <li>1 - feet</li>
	 * <li>2 - meters</li>
	 * <li>3 - Decimal Degrees</li>
	 * <li>4 - Degrees Minutes Seconds </li>
	 */
    int horizontalUnits;
    /**
	 * horizontalDatum  can be one of the following:
	 * <li>0 - unset</li>
	 * <li>1 - NAD83</li>
	 * <li>2 - NAD27</li>
	 * <li>3 - WGS84</li>
	 * <li>4 - WGS72</li>
	 * <li>5 - Local (other)</li>
	 */
    int horizontalDatum;
    /**
	 * Vertical Units can be one of the following:
	 * <li>0 - unspecified</li>
	 * <li>1 - feet</li>
	 * <li>2 - meters</li>
	 */
    int verticalUnits;
    /**
     * verticalDatum  can be one of the following:
     * <ul>
     * <li>0 - unset</li>
     * <li>1 - NAVD88</li>
     * <li>2 - NGVD29</li>
     * <li>3 - Local (other)</li>
     */
    int verticalDatum;

	/*
	*  Location time zone, not necessarily time zone associated with data (maybe GMT?)
	*  e.g., "PST" or "America-Los Angeles".  Never "PDT" (daylight)
	*/
	char *timeZoneName;

	/*
	*	Additional information about the location (NOT data)
	*	This is a null-terminated string
	*	Separate "pieces" of information should be separated by
	*	a new-line character '\n'
	*/
	char *supplemental;

	//  Private - knowing which variables were allocated by the ztsNew functions,
	//  instead of the calling program
	char *pathnameInternal;
	char allocated[zSTRUCT_length];


} zStructLocation;



#endif  //  zSTRUCT_LOCATION_H

