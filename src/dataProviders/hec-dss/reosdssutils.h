#ifndef REOSDSSUTILS_H
#define REOSDSSUTILS_H


#  if defined _WIN32 || defined __CYGWIN__
#    ifdef provider_HEC_DSS_EXPORTS
#      ifdef __GNUC__
#        define REOSDSS_EXPORT __attribute__ ((dllexport))
#      else
#        define REOSDSS_EXPORT __declspec(dllexport) // Note: actually gcc seems to also supports this syntax.
#      endif
#    else
#      ifdef __GNUC__
#        define REOSDSS_EXPORT __attribute__ ((dllimport))
#      else
#        define REOSDSS_EXPORT __declspec(dllimport) // Note: actually gcc seems to also supports this syntax.
#      endif
#    endif
#  else
#    if __GNUC__ >= 4
#      define REOSDSS_EXPORT __attribute__ ((visibility ("default")))
#    else
#      define REOSDSS_EXPORT
#    endif
#  endif

class ReosDuration;
class QString;

class REOSDSS_EXPORT ReosDssUtils
{
  public:
    ReosDssUtils() = default;

    //! Converts a DSS interval string to a ReosDuration instance
    static ReosDuration dssIntervalToDuration( const QString &dssInterval );

    //! Returns the closest valid interval (for DSS) form \a interval
    static ReosDuration closestValidInterval( const ReosDuration &interval );
};

#endif // REOSDSSUTILS_H
