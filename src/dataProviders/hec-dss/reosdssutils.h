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

#include "reosduration.h"

class QString;

class REOSDSS_EXPORT ReosDssUtils
{
  public:
    ReosDssUtils() = default;

    //! Converts a DSS interval string to a ReosDuration instance
    static ReosDuration dssIntervalToDuration( const QString &dssInterval );

    //! Converts a duration interval to a Dss interval string
    static QString durationToDssInterval( const ReosDuration &interval );

    //! Returns the closest valid interval (for DSS) form \a interval
    static ReosDuration closestValidInterval( const ReosDuration &interval );

    static ReosDuration previousValidInterval( const ReosDuration &interval );
    static ReosDuration nextValidInterval( const ReosDuration &interval );

  private:
    static QList<ReosDuration> sValidInterval;

};

#endif // REOSDSSUTILS_H
