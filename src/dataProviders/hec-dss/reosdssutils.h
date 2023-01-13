#ifndef REOSDSSUTILS_H
#define REOSDSSUTILS_H


#  if defined _WIN32 || defined __CYGWIN__
#    ifdef reosHecDss_EXPORTS
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

#include <QComboBox>

#include "reosduration.h"

class QString;
class ReosDssPath;

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

    static QString uri( const QString &filePath, const ReosDssPath &dssPath );

    static const QList<ReosDuration> validIntervals();

    static QString dateToHecRasDate( const QDate &date );

    static QDate dssDateToDate( const QString &dssDate );
    static QTime dssTimeToTime( const QString &dssTime );

  private:
    static QList<ReosDuration> sValidInterval;
};

class REOSDSS_EXPORT ReosDssIntervalCombo : public QComboBox
{
  public:
    ReosDssIntervalCombo( QWidget *parent = nullptr );
    void setInterval( const ReosDuration &duration );
    ReosDuration currentInterval() const;
};

#endif // REOSDSSUTILS_H
