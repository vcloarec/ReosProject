#ifndef __NORMALIZE_F_PART_INCLUDED__
#define __NORMALIZE_F_PART_INCLUDED__

#ifdef __cplusplus
extern "C" {
#endif

	// < 10 : indicates normal F-part with possible ':' and/or '|' characters not intended for tagging
	// = 10 : indicates nothing about the intent of the F-part
	// > 10 : indicates a failed attempt at a tagged F-part
	static struct {
		int SUCCESS;                  //   0
		int NO_TAG_SEPARATOR;         //   1
		int INVALID_TAG_FORMAT;       //   2
		int MULTIPLE_UNTAGGED_PARTS;  //   3
		int EMPTY_F_PART;             //   4
		int MEMORY_ERROR;             //  10
		int INVALID_TAG_CHARACTER;    //  11
		int REPEATED_TAG_CHARACTER;   //  12
		int INVALID_C_TAG_VALUE;      //  13
		int INVALID_T_TAG_VALUE;      //  14
		int INVALID_V_TAG_VALUE;      //  15
		int INVALID_R_TAG_VALUE;      //  16
		int UNEXPECTED_ERROR;         // 100
	} normalizeFPartStatus = {
			  0, // SUCCESS
			  1, // NO_TAG_SEPARATOR
			  2, // INVALID_TAG_FORMAT
			  3, // MULTIPLE_UNTAGGED_PARTS
			  4, // EMPTY_F_PART
			 10, // MEMORY_ERROR
			 11, // INVALID_TAG_CHARACTER
			 12, // REPEATED_TAG_CHARACTER
			 13, // INVALID_C_TAG_VALUE
			 14, // INVALID_T_TAG_VALUE
			 15, // INVALID_V_TAG_VALUE
			 16, // INVALID_R_TAG_VALUE
			100  // UNEXPECTED_ERROR;
	};

	int canUseOriginalFPart(const int errorCode);

	const char* normalizeFPartErrorMessage(const int errorCode);

	/**
	 * Takes a DSS F part string and
	 * <ul>
	 * <li>Ensures only expected tags (C:, T:, V:, N:,and R:) are included</li>
	 * <li>Verifies the data for any included tags are correct</li>
	 * <li>Assembles the resulting F Part with the included tags in canonical order</li>
	 * </ul>
	 * @param normalized The normalized string. NULL on error. Otherwise an allocated string even if identical to input string.
	 *                   You MUST free it yourself.
	 * @param fPart      The F part string to normalize.
	 *
	 * @return
	 * <dl>
	 * <dt>  0 </dt><dd> success                                                      </dd>
	 * <dt>  1 </dt><dd> no ':' found                                                 </dd>
	 * <dt>  2 </dt><dd> ':' found but not in a valid tag format                      </dd>
	 * <dt>  3 </dt><dd> '|' followed by untagged text found more than once           </dd>
	 * <dt>  4 </dt><dd> NULL F-part                                                  </dd>
	 * <dt> 10 </dt><dd> error allocating memory                                      </dd>
	 * <dt> 11 </dt><dd> tag character not in list of valid tag characters            </dd>
	 * <dt> 12 </dt><dd> valid tag character found more than once                     </dd>
	 * <dt> 13 </dt><dd> tag value doesn't match expected format for Collection       </dd>
	 * <dt> 14 </dt><dd> tag value doesn't match expected format for Time of Forecast </dd>
	 * <dt> 15 </dt><dd> tag value doesn't match expected format for Version Time     </dd>
	 * <dt> 16 </dt><dd> tag value doesn't match expected format for Run ID           </dd>
	 * </dl>
	 */
	int normalizeFPart(char** normalized, const char* fPart);

	/**
	 * Takes a DSS pathname and creates a new pathname with the F part normalized
	 * @param normalized The normalized pathname. NULL on error. Otherwise an allocated string even if identical to input pathname.
	 *                   You MUST free it yourself.
	 * @param pathname   The pathname to normalize.
	 *
	 * @return
	 * <dl>
	 * <dt>  0 </dt><dd> success                                                      </dd>
	 * <dt> 10 </dt><dd> error allocating memory                                      </dd>
	 * <dt> 11 </dt><dd> tag character not in list of valid tag characters            </dd>
	 * <dt> 12 </dt><dd> valid tag character found more than once                     </dd>
	 * <dt> 13 </dt><dd> tag value doesn't match expected format for Collection       </dd>
	 * <dt> 14 </dt><dd> tag value doesn't match expected format for Time of Forecast </dd>
	 * <dt> 15 </dt><dd> tag value doesn't match expected format for Version Time     </dd>
	 * <dt> 16 </dt><dd> tag value doesn't match expected format for Run ID           </dd>
	 * </dl>
	 */
	int normalizeFPartInPathname(char** normalized, const char* pathname);

#ifdef __cplusplus
}
#endif
#endif //__NORMALIZE_F_PART_INCLUDED__