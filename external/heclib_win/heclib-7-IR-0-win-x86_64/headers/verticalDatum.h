#ifndef __VERTICAL_DATUM_INFO_INCLUDED__
#define __VERTICAL_DATUM_INFO_INCLUDED__

#ifdef __cplusplus
extern "C" {
#endif
 
#include <float.h>
#include <stdint.h>
#include <zStructLocation.h>
 
#define F2C(f, c, flen, clen) {                    \
    int len = flen < clen ? flen : clen - 1;       \
    strncpy(c, f, len);                            \
    c[len] = '\0';                                 \
    for (int i = len-1; i > -1; --i) {             \
        if (c[i] != ' ' ) break;                   \
        c[i] = '\0';                               \
    }                                              \
}
#define C2F(c, f, flen) {                          \
    if (c == NULL) {                               \
        for (int i = 0; i < flen; ++i) f[i] = ' '; \
    }                                              \
    else {                                         \
        int i = strlen(c);                         \
        int len = i > flen ? flen : i;             \
        strncpy(f, c, len);                        \
        for (i = len; i < flen; ++i) f[i] = ' ';   \
    }                                              \
}

#define BYTESWAP(d)                     \
{                                       \
    uint8_t* b1 = (uint8_t* )&d, b2[8]; \
    switch(sizeof(d))                   \
    {                                   \
        case 1:                         \
            break;                      \
                                        \
        case 2:                         \
            b2[0] = b1[1];              \
            b2[1] = b1[0];              \
                                        \
            b1[0] = b2[0];              \
            b1[1] = b2[1];              \
            break;                      \
                                        \
        case 4:                         \
            b2[0] = b1[3];              \
            b2[1] = b1[2];              \
            b2[2] = b1[1];              \
            b2[3] = b1[0];              \
                                        \
            b1[0] = b2[0];              \
            b1[1] = b2[1];              \
            b1[2] = b2[2];              \
            b1[3] = b2[3];              \
            break;                      \
                                        \
        case 8:                         \
            b2[0] = b1[7];              \
            b2[1] = b1[6];              \
            b2[2] = b1[5];              \
            b2[3] = b1[4];              \
            b2[4] = b1[3];              \
            b2[5] = b1[2];              \
            b2[6] = b1[1];              \
            b2[7] = b1[0];              \
                                        \
            b1[0] = b2[0];              \
            b1[1] = b2[1];              \
            b1[2] = b2[2];              \
            b1[3] = b2[3];              \
            b1[4] = b2[4];              \
            b1[5] = b2[5];              \
            b1[6] = b2[6];              \
            b1[7] = b2[7];              \
            break;                      \
                                        \
        default:                        \
            assert(0);                  \
            break;                      \
    }                                   \
}

//----------------// 
// misc constants //
//----------------// 
#define FALSE 0
#define TRUE  1
#define UNDEFINED_VERTICAL_DATUM_VALUE -FLT_MAX
#define METERS_PER_FOOT 0.3048
#define CVERTICAL_DATUM_SIZE 17 // with NULL terminator
#define UNIT_SIZE 17 // with NULL terminator
//-----------------------------//
// Vertical datum constants    //
//                             //
// Same order as DataContainer //
//-----------------------------//
#define IVERTICAL_DATUM_UNSET  0
#define IVERTICAL_DATUM_NAVD88 1
#define IVERTICAL_DATUM_NGVD29 2
#define IVERTICAL_DATUM_OTHER  3
#define IVERTICAL_DATUM_LOCAL  IVERTICAL_DATUM_OTHER

#define CVERTICAL_DATUM_UNSET  "UNSET"
#define CVERTICAL_DATUM_NAVD88 "NAVD-88"
#define CVERTICAL_DATUM_NGVD29 "NGVD-29"
#define CVERTICAL_DATUM_OTHER  "OTHER"
#define CVERTICAL_DATUM_LOCAL  CVERTICAL_DATUM_OTHER
//-----------------------------------------------------//
// parameter constants for global values (zset/zquery) //
//-----------------------------------------------------//
#define VERTICAL_DATUM_USER_HEADER_PARAM "verticalDatum"
#define VERTICAL_DATUM_USER_HEADER_PARAM_LEN 13 //strlen(VERTICAL_DATUM_USER_HEADER_PARAM)
#define VERTICAL_DATUM_INFO_USER_HEADER_PARAM "verticalDatumInfo"
#define VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN 17 //strlen(VERTICAL_DATUM_INFO_USER_HEADER_PARAM)

//----------------//
// error messages //
//----------------//
#define NULL_POINTER_ERROR                              "Null pointer error"
#define INSUFFICIENT_SPACE_ERROR                        "Not enough space for data"
#define MEMORY_ALLOCATION_ERROR                         "Memory allocation error"
#define BASE64_DECODING_ERROR                           "Base64 decoding error"
#define BASE64_ENCODING_ERROR                           "Base64 encoding error"
#define BEGINNING_BOUNDARY_NOT_FOUND                    "Beginning boundary not found"
#define ENDING_BOUNDARY_NOT_FOUND                       "Ending boundary not found"
#define ERROR_ON_DEFLATE                                "Error on Deflate())"
#define ERROR_ON_DEFLATEEND                             "Error on DeflateEnd()"
#define ERROR_ON_DEFLATEINIT2                           "Error on DeflateInit2()"
#define ERROR_ON_INFLATE                                "Error on Inflate()"
#define ERROR_ON_INFLATEEND                             "Error on InflateEnd()"
#define ERROR_ON_INFLATEINIT2                           "Error on InflateInit2()"
#define INPUT_STRING_IS_NULL                            "Input string is NULL"
#define INVALID_DATUM_IN_SPECIFIED_IN_XML               "Invalid datum in <to-datum>...</to-datum> block in XML"
#define INVALID_OFFSET_UNIT_IN_XML                      "Invalid offset unit in XML"
#define INVALID_OFFSET_VALUE_IN_XML                     "Invalid offset value in XML"
#define INVALID_NATIVE_DATUM_IN_XML                     "Invalid native datum in XML"
#define INVALID_NAVD_88_OFFSET_BLOCK_IN_XML             "Invalid NAVD-88 offset block in XML"
#define INVALID_NAVD_88_OFFSET_VALUE_IN_XML             "Invalid NAVD-88 offset value in XML"
#define INVALID_NGVD_29_OFFSET_BLOCK_IN_XML             "Invalid NGVD-29 offset block in XML"
#define INVALID_NGVD_29_OFFSET_VALUE_IN_XML             "Invalid NGVD-29 offset value in XML"
#define MISSING_OFFSET_BLOCK_IN_XML                     "Missing <offset>...</offset> block in XML"
#define INVALID_OFFSET_BLOCK_IN_XML                     "Invalid <offset>...</offset> block in XML"
#define INVALID_XML_STRUCTURE                           "Invalid XML Structure"
#define MULTIPLE_NAVD_88_OFFSET_BLOCKS_IN_XML           "Multiple NAVD-88 offset blocks in XML"
#define MULTIPLE_NGVD_29_OFFSET_BLOCKS_IN_XML           "Multiple NGVD-29 offset blocks in XML"
#define NO_OFFSET_UNIT_IN_XML                           "No offset unit in XML"
#define NO_NATIVE_DATUM_IN_XML                          "No native datum in XML"
#define NO_NAVD_88_OFFSET_VALUE_IN_XML                  "No NAVD-88 offset value in XML"
#define NO_NGVD_29_OFFSET_VALUE_IN_XML                  "No NGVD-29 offset value in XML"
#define XML_IS_NOT_A_VALID_VERTICAL_DATUM_INFO_INSTANCE "XML is not a valid vertical datum info instance"
#define XML_IS_NOT_WELL_FORMED                          "XML is not well formed"

#ifdef _MSC_VER 
    #define strcasecmp stricmp
    #define strncasecmp strnicmp
    #define strtok_r strtok_s
#endif

#if defined(__GNUC__) || defined(__sun__)
    #define MIN(a, b) ({         \
        __typeof__ (a) _a = (a); \
        __typeof__ (b) _b = (b); \
        _a < _b ? _a : _b;       \
    })
#else
    #define MIN(a, b) min(a,b)
#endif

typedef struct verticalDatumInfo_s {
    char   nativeDatum[CVERTICAL_DATUM_SIZE];
    char   unit[UNIT_SIZE];
    double offsetToNgvd29;
    int    offsetToNgvd29IsEstimate;
    double offsetToNavd88;
    int    offsetToNavd88IsEstimate;
} verticalDatumInfo;

typedef struct textBoundaryInfo_s {
    char* first;
    char* firstNonBlank;
    char* firstWithBoundary;
    char* last;
    char* lastNonBlank;
    char* lastWithBoundary;
    int   offset;
    int   offsetNonBlank;
    int   len;
    int   lenNonBlank;
    int   lenWithBoundaries;
} textBoundaryInfo;
/**
 * Returns TRUE if unit recognized as feet, otherwise FALSE
 */
int unitIsFeet(const char* unit);
/**
 * Returns TRUE if unit recognized as meters, otherwise FALSE
 */
int unitIsMeters(const char* unit);
/**
 * Returns a vertical datum offset adjusted for data unit
 * 
 * @param offset     The value of the offset
 * @param offsetUnit The unit of offset
 * @param dataUnit   The unit of the elevations being modified
 * 
 * @return The offset appropriate for adjusting the data values with. Returns UNDEFINED_VERTICAL_DATUM_VALUE
 *         if either of the units cannot be identified as feet or meters
 */
double getOffset(double offset, const char* offsetUnit, const char* dataUnit);
/**
 * Returns a parameter value from a string whose parts are delimited by a single character, optionally 
 * removing the parameter and value from the string
 * 
 * @param delimitedStr     The delimited string (possibly modified) if removeFromString is TRUE
 * @param parameter        The parameter to extract the value for
 * @param separator        A string that separates the parameter from the value. May be NULL
 * @param matchCase        A flag to specify whether to use case sensitive matching on the parameter
 * @param removeFromString A flag to specify whether to remove the parameter and value from the delimited string
 * @param delimiter        The character used as a delimiter
 * 
 * @return The value of the parameter in the user header string, or NULL if the parameter is not found. Memory for 
 *         this buffer is dynamically allocated using malloc() and must be freed using free() by the caller if this 
 *         call is successful.
 */
char* extractFromDelimitedString(
    char**      delimitedStr, 
    const char* parameter, 
    const char* separator,
    int         matchCase, 
    int         removeFromString, 
    char        delimiter);
/**
 * Inserts a parameter/value pair into a string whose parts are delimited by a single character
 * 
 * @param delimitedString      The delimited string to modify
 * @param delimitedStringSize  The allocated size in bytes of the delimited string
 * @param parameter            The parameter to add to the delimited string
 * @param separator            A string that separates the parameter from the value. May be NULL
 * @param value                The value of the parameter to add to the delimited string. May be NULL 
 * @param overwriteExisting    A flag to specify overwriting the value of the parameter if it already exists
 * @param delimiter            The character used as a delimiter
 * 
 * @return 0 on success - either the parameter existed and overwriteExisting was FALSE, or the user header sting 
 *         was successfully modified. -1 if there is not enough room to insert the parameter/value pair
 * 
 */
int insertIntoDelimitedString(
    char**      delimitedString, 
    int         delimitedStringSize, 
    const char* parameter, 
    const char* value, 
    const char* separator, 
    int         overwriteExisting, 
    char        delimiter);
/**
 * Returns a string representation of a DSS record user header
 * 
 * @param userHeader     The user header (integer array)
 * @param userHeaderNumber The size of the user header in number of integers
 * 
 * @return The string representation of the user header. Memory for this buffer 
 *         is dynamically allocated using malloc() and must be freed using free().
 */
char* userHeaderToString(const int* userHeader, int userHeaderNumber);
/**
 * Creates a DSS record user header from a string
 * 
 * @param str            The string to create the user header from
 * @param userHeaderNumber Pointer to an int that receives the number of integers allocated.
 *                       This should always be (len_str - 1) / 4 + 1.
 * 
 * @return The user header (array of integers).  Memory for this buffer is dynamically 
 *         allocated using malloc() and must be freed using free() unless it is a member
 *         zStructTime* structure. In that case the "allocated" structure member should
 *         be set to 1 to all zStructFree() to free the memory.
 */
int* stringToUserHeader(const char* str, int* userHeaderNumber);
/**
 * Returns the length of a buffer required to hold a base-64 encoding of an input buffer of the specified length.
 *
 * @param toEncodeLen The length of the buffer to base64-encode
 *
 * @return -1 if toEncodeLen < 0, otherwise the length of buffer necessary to hold the base-64 encoding.
 *         Always a multiple of 4. 
 */
int b64EncodedLen(int toEncodeLen);
/**
 * Returns the length of a buffer required to hold a base-64 decoding of an input buffer of the specified length.
 *
 * @param toDecodeLen The length of the buffer to base64-decode. Must be a multiple of 4
 *
 * @return -1 if to_deocde_len < 0, -2 if toDecodeLen is not a multiple of 4, otherwise the length of buffer
 *         necessary to hold the base-64 decoding. Any actual decoded length may be one or two bytes shorder
 *         depending on the actual input buffer (it will be shorter by the number of '=' characters at the end
 *         of the data to base64-decode).
 */
int b64DecodedLen(int toDecodeLen);
/**
 * Base64-encodes a buffer
 *
 * @param encoded     The encoded version of toEncode. Memory for this buffer is dynamically allocated
 *                    using malloc() and must be freed using free() by the caller if this call is successful.
 *                    This buffer is always a null-terminated string.
 * @param toEncode    A pointer to the buffer to encode
 * @param toEncodeLen The length of the portion of toEncode to encode.
 *
 * @return -1 if toEncodeLen < 0, otherwise 0 on success
 */
int b64Encode(char** encoded, const char* toEncode, int toEncodeLen);
/**
 * Base64-decodes a buffer
 *
 * @param decoded    The decoded data. Memory for this buffer is dynamically allocated using malloc()
 *                   and must be freed using free() by the caller if this call is successful.
 * @param decodedLen A pointer to an integer to receive the actual decoded length. This may be slightly less
 *                   than the value retuned by b64EncodedLen(strlen(toDecode))
 * @param toDecode   A null-terminated string of the base64-encoded data
 *
 * @return -2 if strlen(toDecode) is not a multiple of 4, -3 if any character in toDecode is not a valie
 *         base64 encoding character, otherwise 0 on success
 */
int b64Decode(char** decoded, int* decodedLen, const char* toDecode);
 
/**
 * Locates the first occurrence of text in buffer that is bounded by specified text strings;
 *
 * @param tbi    A pointer to an existing textBoundaryInfo structure to hold the the results
 * @param buf    The buffer to search in
 * @param after  The first boundary (the located text is immediately after this)
 * @param before The second boundary (the located text is immediately before this)
 *
 * @return An error message, which is NULL the function succeeds.
 */
char* findTextBetween(textBoundaryInfo* tbi, const char* buf, const char* after, const char* before);
/**
 *  Returns a text string that has been gzipped and then base64 encoded to its original state
 *
 * @param results   The decoded and un-gzipped string. Memory for this buffer is dynamically allocated
 *                  using malloc() and must be freed using free() by the caller.
 * @param inputBuf The buffer to decode and un-gzip.
 *
 * @return An error message, which is NULL if the function succeeds.
 */
char* decodeAndGunzip(char** results, const char* inputBuf);
/**
 * Gzips and base64 encodes a text string
 *
 * @param results   The gzipped and encoded string. Memory for this buffer is dynamically allocated
 *                  using malloc() and must be freed using free() by the caller.
 * @param inputBuf The buffer to gzip and encode.
 *
 * @return An error message, which is NULL if the function succeeds.
 */
char* gzipAndEncode(char** results, const char* inputBuf);
/**
 * Expands empty tags of the format <tag_name/> to <tag_name></tag_name> for purpose of easy
 * structure validation.
 *
 * @param outputBuf The result of the expansion. Memory for this buffer is dynamically allocated
 *                  using malloc() and must be freed using free() by the caller.
 * @param inputBuf  The XML instance to expand
 *
 * @return An error message, which is NULL the function succeeds.
 */
char* expandEmptyXmlTags(char** outputBuf, const char* inputBuf);
/**
 * Validates the well-formedness of an XML instance
 *
 * @param xml The XML instance to verify
 *
 * @return An error message, which is NULL the function succeeds.
 */
char* validateXmlStructure(const char* xml);
/**
 * initializes a verticalDatumInfo structure
 *
 * @param vdi       A ponter to a previously existing verticalDatumInfo sturcture
 */
void initializeVerticalDatumInfo(verticalDatumInfo* vdi);
/**
 * Parses a standard vertical datum infomration XML instance into data structure
 *
 * @param vdi       A ponter to a previously existing verticalDatumInfo sturcture
 * @param intputStr The XML instance to parse. This may be either a plain text XML instance or one
 *                  that has been gzipped and base64 encoded
 */
char* stringToVerticalDatumInfo(verticalDatumInfo* vdi, const char* inputStr);
/**
 * Creates a (compressed or uncompressed) string from vertical datum information
 *
 * @param results             The string containing the vertical datum infomation . Memory for this buffer is
 *                            dynamically allocated using malloc() and must be freed using free() by the caller.
 * @param vdi                 A ponter to a previously existing verticalDatumInfo sturcture containing the information
 * @param generate_compressed A flag (TRUE/FALSE) that specifies whether to generate a compressed string.
 *                            If FALSE, the result will be an XML instance containing the vertical datum information.
 *                            If TRUE, the XML will be gzipped and base64 encoded.
 *
 * @return An error message, which is NULL the function succeeds.
 */
char* verticalDatumInfoToString(char** results, const verticalDatumInfo* vdi, int generate_compressed);
/**
 * Returns any vertical datum info in a DSS record user header
 * 
 * @param userHeader     The user header (integer array)
 * @param userHeaderSize The size of the user header in number of integers
 * 
 * @return A pointer to a dynmically allocated verticalDatumInfo struct, or NULL if the user header doesn't
 *         include any vertical datum info
 */
verticalDatumInfo* extractVerticalDatumInfoFromUserHeader(const int* userHeader, int userHeaderSize);
/**
 * Retrieves the effective vertical datum from the current environment. The effective vertical datum is
 * set from the following locations, in increasing priority
 * <p>
 * <ol>
 * <li> The global parameter "VDTM" set with zset()</li>
 * <li> The user header value of the "verticalDatum" parameter, if it exists</li>
 * <li> The value of the "V" key in a unit specification of the form "U=unit|V=vertical_datum"</li>
 * </ol>
 * </p><p>
 * If the vertical datum is specified in the user header (even if the datum is ultimately taken from
 * the unit specification) the header is modified to remove it.
 * </p><p>
 * If the vertical datum is taken from the unit specification, the unit parameter is modified to 
 * contain only the value of the "U" key.
 * 
 * @param cverticalDatum     Receives the character version of the effective vertical datum
 * @param cverticalDatumSize Size of the buffer in cverticalDatum parameter
 * @param userHeader         User header array. The contents may be modified by the routine but stack buffers are safe to pass in.
 * @param userHeaderSize     The number of integers used for data in the userHeader parameter. The may be modified by the routine.
 * @param unit               The unit specification. The contents may be modified by the routine but stack buffers are safe to pass in.
 * @return                   The integer version of the effective vertical datum or -1 on error.
 */
int	getCurrentVerticalDatum(
    char* cverticalDatum,
    int   cverticalDatumSize,
    int* userHeader,
    int*  userHeaderSize,
    char* unit);
/**
 * Normalizes the VDI representation in the user header
 * 
 * @param userHeader       the integer array
 * @param userHeaderNumber the number of integers used in the array
 * @param userHeaderSize   the max number of integers for the array
 * 
 * @return NULL on success, otherwise an error message
 */
char* normalizeVdiInUserHeader(int* userHeader, int* userHeaderNumber, int userHeaderSize);
/**
 * Fortan wrapper for stringToVerticalDatumInfo
 *
 * Use the following Fortran interface for this routine:
 *
 *  interface
 *      subroutine stringToVerticalDatumInfo( &
 *          inputStr,                         &
 *          errorMessage,                     &
 *          nativeDatum,                      &
 *          unit,                             &
 *          offsetNgvd29,                     &
 *          offsetNgvd29IsEstimate,           &
 *          offsetNavd88,                     &
 *          offsetNavd88IsEstimate)
 *          character (len = *),  intent(in)  :: inputStr
 *          character (len = *),  intent(out) :: errorMessage
 *          character (len = *),  intent(out) :: nativeDatum
 *          character (len = *),  intent(out) :: unit
 *          real      (kind = 8), intent(out) :: offsetNgvd29
 *          logical   (kind = 4), intent(out) :: offsetNgvd29IsEstimate
 *          real      (kind = 8), intent(out) :: offsetNavd88
 *          logical   (kind = 4), intent(out) :: offsetNavd88IsEstimate
 *      end subroutine stringToVerticalDatumInfo
 *  end interface
 *
 * @param inputStr               Fortran CHARACTER (LEN=*) input for input XML in raw or compressed format.
 * @param errorMessage           Fortran CHARACTER (LEN=*) output for error message. Empty on success. Length should be >= 64
 * @param nativeDatum            Fortran CHARACTER (LEN=*) output for native datum. Length should be >= 16.
 * @param unit                   Fortran CHARACTER (LEN=*) output for unit of offsets. Length should be >= 2
 * @param offsetNgvd29           Fortran REAL (KIND=4) output for the offset to NGVD-29. UNDEFINED_VERTICAL_DATUM_VALUE if no value in XML
 * @param offsetNgvd29IsEstimate Fortran LOGICAL (KIND=4) output for whether the offset to NGVD-29 is estimated
 * @param offsetNavd88           Fortran REAL (KIND=4) output for the offset to NAVD-88. UNDEFINED_VERTICAL_DATUM_VALUE if no value in XML
 * @param offsetNavd88IsEstimate Fortran LOGICAL (KIND=4) output for whether the offset to NAVD-88 is estimated
 * @param lenInputStr            Fortran hidden parameter for declared length of inputStr parameter
 * @param lenErrorMessage        Fortran hidden parameter for declared length of error_message parameter
 * @param lenNativeDatum         Fortran hidden parameter for declared length of nativeDatum parameter
 * @param lenUnit                Fortran hidden parameter for declared length of unit parameter
 */
void stringtoverticaldatuminfo_(
        const char* inputStr,
        char*       errorMessage,
        char*       nativeDatum,
        char*       unit,
        double*     offsetNgvd29,
        int32_t*    offsetNgvd29IsEstimate,
        double*     offsetNavd88,
        int32_t*    offsetNavd88IsEstimate,
        size_t      lenInputStr,
        size_t      lenErrorMessage,
        size_t      lenNativeDatum,
        size_t      lenUnit);
/**
 * Fortan wrapper for verticalDatumInfoToString
 *
 * Use the following Fortran interface for this routine:
 *
 *  interface
 *      subroutine verticalDatumInfoToString( &
 *          outputStr,                        &
 *          errorMessage,                     &
 *          nativeDatum,                      &
 *          unit,                             &
 *          offsetNgvd29,                     &
 *          offsetNgvd29IsEstimate,           &
 *          offsetNavd88,                     &
 *          offsetNavd88IsEstimate,           &
 *          generateCompressed)
 *          character (len = *),  intent(out) :: outputStr
 *          character (len = *),  intent(out) :: errorMessage
 *          character (len = *),  intent(in)  :: nativeDatum
 *          character (len = *),  intent(in)  :: unit
 *          real      (kind = 8), intent(in)  :: offsetNgvd29
 *          logical   (kind = 4), intent(in)  :: offsetNgvd29IsEstimate
 *          real      (kind = 8), intent(in)  :: offsetNavd88
 *          logical   (kind = 4), intent(in)  :: offsetNavd88IsEstimate
 *          logical   (kind = 4), intent(in)  :: generateCompressed
 *      end subroutine verticalDatumInfoToString
 *  end interface
 *
 * @param outputStr              Fortran CHARACTER (LEN=*) output in raw (XML) or compressed format. Length should be >= 400
 * @param errorMessage           Fortran CHARACTER (LEN=*) output for error message. Empty on success. Length should be >= 64
 * @param nativeDatum            Fortran CHARACTER (LEN=*) input for native datum.
 * @param unit                   Fortran CHARACTER (LEN=*) input for unit of offsets.
 * @param offsetNgvd29           Fortran REAL (KIND=4) input for the offset to NGVD-29. UNDEFINED_VERTICAL_DATUM_VALUE if n/a
 * @param offsetNgvd29IsEstimate Fortran LOGICAL (KIND=4) input for whether the offset to NGVD-29 is estimated
 * @param offsetNavd88           Fortran REAL (KIND=4) input for the offset to NAVD-88. UNDEFINED_VERTICAL_DATUM_VALUE if n/a
 * @param offsetNavd88IsEstimate Fortran LOGICAL (KIND=4) input for whether the offset to NAVD-88 is estimated
 * @param generateCompressed     Fortran LOGICAL (KIND=4) input for whether to generate compressed or raw (XML) string
 * @param len_output_str         Fortran hidden parameter for declared length of output_str parameter
 * @param len_error_message      Fortran hidden parameter for declared length of error_message parameter
 * @param len_native_datum       Fortran hidden parameter for declared length of nativeDatum parameter
 * @param len_unit               Fortran hidden parameter for declared length of unit parameter
 */
void verticaldatuminfotostring_(
        char*          outputStr,
        char*          errorMessage,
        const char*    nativeDatum,
        const char*    unit,
        const double*  offsetNgvd29,
        const int32_t* offsetNgvd29IsEstimate,
        const double*  offsetNavd88,
        const int32_t* offsetNavd88IsEstimate,
        const int32_t* generateCompressed,
        size_t         lenErrorMessage,
        size_t         lenOutputStr,
        size_t         lenNativeDatum,
        size_t         lenUnit);
/**
 * Fortran wrapper for normalizeVdiInUserHeader
 */
void normalizevdiinuserheader_(
    int*   userHeader, 
    int*   userHeaderNumber, 
    int*   userHeaderSize,
    char*  errorMesage, 
    size_t lenErrorMessage);

/**
 * Processes VDIs for storing to DSS
 * @param offsetToUse      Receives the offset to use on the data values before storing 
 * @param fileVdi         VDI on disk for records to be stored
 * @param dataVdi         VDI of incoming data
 * @param currentDatum    Current datum of the incoming data values
 * @param fileContainsData Whether the records to be stored have existing data (0/1)
 * @param dataUnit         Unit of the incoming data values
 * @return
 */
char* processStorageVdis(
    double*                  offsetToUse,
    const verticalDatumInfo* fileVdi,
    const verticalDatumInfo* dataVdi,
    const char*              currentDatum,
    int                      fileContainsData,
    const char*              dataUnit);

/**
 * Copies any VDI from a location struct to a user header. Used for copying DSS 7 records to DSS 6
 * @param locStruct        The location struct 
 * @param userHeader       The user header integer array
 * @param userHeaderNumber The number of ints in the user header
 * @param status           Receives 0 on success or < 0 on error
 * @param freeOriginalHeader Flag to specify whether to free the original header if a new header is allocated
 *                           in order to provide enough space for the VDI
 * @return The (possibly reallocated) user header on success
 */
int* copyVdiFromLocationStructToUserHeader(
    zStructLocation* locStruct,
    int* userHeader,
    int* userHeaderNumber,
    int freeOriginalHeader,
    int* status);

/**
 * Returns whether a DSS pathname is an elevation time series. Really only test whether the C pathname
 * part is "Elev"; no other tests are performed.
 * 
 * @param pathname The pathname to test
 * @return whether the C pathname part starts with "Elev" (case insensitive)
 */
int pathnameIsElevTs(const char* pathname);

/**
 * Returns whether a DSS pathname is paired data with elevation in independent or dependent parameter. Really only test the C pathname
 * part to see if either parameter starts with "Elev"; no other tests are performed.
 *
 * @param pathname The pathname to test
 * @return <ul>
 *         <li><b>0</b> if neither parameter is elevation</li>
 *         <li><b>1</b> if only independent parameter is elevation</li>
 *         <li><b>2</b> if only dependent parameter is elevation</li>
 *         <li><b>3</b> if both independent and dependent parameters are elevation</li>
 *         </ul>
 */
int pathnameIsElevPd(const char* pathname);

/**
 * Returns whether a DSS pathname is an elevation time sereis or paired data. Really only tests the C pathname part
 *
 * @param pathname The pathname to test
 * @return whether either parameter in the C pathname part starts with "Elev" (case insensitive)
 */
int pathnameIsElevTsOrPd(const char* pathname);

#ifdef __cplusplus
} // extern "C"
#endif
 
#endif //__VERTICAL_DATUM_INFO_INCLUDED__
