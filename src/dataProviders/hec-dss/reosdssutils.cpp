#include "reosdssutils.h"

#include <QDate>

extern "C" {
#include "heclib.h"
}


#include "reosduration.h"
#include "reosdssfile.h"


ReosDuration ReosDssUtils::dssIntervalToDuration( const QString &dssInterval )
{
  int interValSeconds = 0;
  int flagDirection = 1;

  if ( !dssInterval.isEmpty() &&
       ( ztsGetStandardInterval( 7,
                                 &interValSeconds,
                                 const_cast<char *>( dssInterval.toStdString().c_str() ),
                                 static_cast<size_t>( dssInterval.size() + 1 ),
                                 &flagDirection ) == STATUS_OKAY ) )
  {
    return ReosDuration( interValSeconds, ReosDuration::second );
  }

  return ReosDuration();
}

QString ReosDssUtils::durationToDssInterval( const ReosDuration &interval )
{
  std::vector<char> strVector;
  strVector.resize( 32 );
  int interValSeconds = int ( interval.valueSecond() + 0.5 );
  int flagDirection = 2;

  if ( ztsGetStandardInterval( 7,
                               &interValSeconds,
                               strVector.data(),
                               32,
                               &flagDirection ) == STATUS_OKAY )
  {
    return QString( strVector.data() );
  }

  return QString();

}
QList<ReosDuration> ReosDssUtils::sValidInterval =
{
  ReosDuration( 1, ReosDuration::second )
  , ReosDuration( 2, ReosDuration::second )
  , ReosDuration( 3, ReosDuration::second )
  , ReosDuration( 4, ReosDuration::second )
  , ReosDuration( 5, ReosDuration::second )
  , ReosDuration( 6, ReosDuration::second )
  , ReosDuration( 10, ReosDuration::second )
  , ReosDuration( 15, ReosDuration::second )
  , ReosDuration( 20, ReosDuration::second )
  , ReosDuration( 30, ReosDuration::second )
  , ReosDuration( 1, ReosDuration::minute )
  , ReosDuration( 2, ReosDuration::minute )
  , ReosDuration( 3, ReosDuration::minute )
  , ReosDuration( 4, ReosDuration::minute )
  , ReosDuration( 5, ReosDuration::minute )
  , ReosDuration( 6, ReosDuration::minute )
  , ReosDuration( 10, ReosDuration::minute )
  , ReosDuration( 12, ReosDuration::minute )
  , ReosDuration( 15, ReosDuration::minute )
  , ReosDuration( 20, ReosDuration::minute )
  , ReosDuration( 30, ReosDuration::minute )
  , ReosDuration( 1, ReosDuration::hour )
  , ReosDuration( 2, ReosDuration::hour )
  , ReosDuration( 3, ReosDuration::hour )
  , ReosDuration( 4, ReosDuration::hour )
  , ReosDuration( 6, ReosDuration::hour )
  , ReosDuration( 8, ReosDuration::hour )
  , ReosDuration( 12, ReosDuration::hour )
  , ReosDuration( 1, ReosDuration::day )
  , ReosDuration( 1, ReosDuration::week )
  , ReosDuration( 10, ReosDuration::day )
  , ReosDuration( 15, ReosDuration::day )
  , ReosDuration( 1, ReosDuration::month )
  , ReosDuration( 1, ReosDuration::year )
};

ReosDuration ReosDssUtils::closestValidInterval( const ReosDuration &interval )
{
  for ( int i = 0; i < sValidInterval.count() - 1; ++i )
  {
    const ReosDuration &vi1 =  sValidInterval.at( i );
    const ReosDuration &vi2 =  sValidInterval.at( i + 1 );

    if ( interval <= vi1 )
      return vi1;

    if ( interval == vi2 )
      return vi2;

    if ( interval > vi1 && interval < vi2 )
    {
      if ( interval - vi1 < vi2 - interval )
        return vi1;
      else
        return vi2;
    }
  }

  return sValidInterval.last();
}

ReosDuration ReosDssUtils::previousValidInterval( const ReosDuration &interval )
{
  for ( int i = 0; i < sValidInterval.count() - 1; ++i )
  {
    const ReosDuration &vi1 =  sValidInterval.at( i );
    const ReosDuration &vi2 =  sValidInterval.at( i + 1 );

    if ( interval <= vi1 )
      return ReosDuration();

    if ( interval == vi2 )
      return vi1;

    if ( interval > vi1 && interval < vi2 )
    {
      return vi1;
    }
  }

  return sValidInterval.last();
}

ReosDuration ReosDssUtils::nextValidInterval( const ReosDuration &interval )
{
  for ( int i = 0; i < sValidInterval.count() - 1; ++i )
  {
    const ReosDuration &vi1 =  sValidInterval.at( i );
    const ReosDuration &vi2 =  sValidInterval.at( i + 1 );

    if ( interval < vi1 )
      return vi1;

    if ( interval == vi1 )
      return vi2;

    if ( interval > vi1 && interval < vi2 )
    {
      return vi2;
    }
  }

  return ReosDuration();
}

QString ReosDssUtils::uri( const QString &filePath, const ReosDssPath &dssPath )
{
  ReosDssPath pathWithoutDate = dssPath;
  pathWithoutDate.setStartDate( QString() );

  return QStringLiteral( "\"%1\"::%2" ).arg( filePath, dssPath.string() );
}

const QList<ReosDuration> ReosDssUtils::validIntervals()
{
  return sValidInterval;
}

QString ReosDssUtils::dateToHecRasDate( const QDate &date )
{

  if ( date.isNull() || !date.isValid() )
    return QString();

  QString monthStr;
  switch ( date.month() )
  {
    case 1:
      monthStr = QStringLiteral( "jan" );
      break;
    case 2:
      monthStr = QStringLiteral( "feb" );
      break;
    case 3:
      monthStr = QStringLiteral( "mar" );
      break;
    case 4:
      monthStr = QStringLiteral( "apr" );
      break;
    case 5:
      monthStr = QStringLiteral( "may" );
      break;
    case 6:
      monthStr = QStringLiteral( "jun" ) ;
      break;
    case 7:
      monthStr = QStringLiteral( "jul" );
      break;
    case 8:
      monthStr = QStringLiteral( "aug" ) ;
      break;
    case 9:
      monthStr = QStringLiteral( "sep" );
      break;
    case 10:
      monthStr = QStringLiteral( "oct" ) ;
      break;
    case 11:
      monthStr = QStringLiteral( "nov" ) ;
      break;
    case 12:
      monthStr = QStringLiteral( "dec" ) ;
      break;
  }

  QString day = QString::number( date.day() );
  if ( day.count() == 1 )
    day.prepend( '0' );

  QString year = QString::number( date.year() );

  return day + monthStr + year;
}

QDate ReosDssUtils::dssDateToDate( const QString &dssDate )
{
  int y = -1, m = -1, d = -1;

  int res = dateToYearMonthDay( dssDate.toUtf8().data(), &y, &m, &d );
  if ( res == STATUS_OKAY )
  {
    return QDate( y, m, d );
  }

  return QDate();
}

QTime ReosDssUtils::dssTimeToTime( const QString &dssTime )
{
  double seconds = timeStringToSecondsMills( dssTime.toUtf8().data() );
  if ( seconds < 0 )
    return QTime();

  return QTime::fromMSecsSinceStartOfDay( static_cast<int>( seconds * 1000 + 0.5 ) );
}

QString ReosDssUtils::dssProviderKey()
{
  return QStringLiteral( "dss" );
}

#if QT_VERSION < QT_VERSION_CHECK(5, 15, 0)
#define skipEmptyPart QString::SkipEmptyParts
#else
#define skipEmptyPart Qt::SplitBehaviorFlags::SkipEmptyParts
#endif

QString ReosDssUtils::dssFileFromUri( const QString &uri )
{
  if ( !uri.contains( QStringLiteral( "\"" ) ) )
    return uri;

  QStringList split = uri.split( QStringLiteral( "\"" ), skipEmptyPart );
  return split.at( 0 );
}

ReosDssPath ReosDssUtils::dssPathFromUri( const QString &uri )
{
  if ( !uri.contains( QStringLiteral( "::" ) ) )
    return ReosDssPath( QString() );

  const QStringList parts = uri.split( QStringLiteral( "::" ) );
  if ( parts.count() > 1 )
    return ReosDssPath( parts.at( 1 ) );

  return ReosDssPath( QString() );
}

ReosDssIntervalCombo::ReosDssIntervalCombo( QWidget *parent ) : QComboBox( parent )
{
  for ( const ReosDuration &interval : ReosDssUtils::validIntervals() )
  {
    addItem( interval.toString( 0 ), interval.valueMilliSecond() );
  }
}

void ReosDssIntervalCombo::setInterval( const ReosDuration &duration )
{
  ReosDuration valid = ReosDssUtils::closestValidInterval( duration );
  int index = findData( valid.valueMilliSecond() );
  setCurrentIndex( index );
}

ReosDuration ReosDssIntervalCombo::currentInterval() const
{
  return ReosDuration( currentData().toLongLong(), ReosDuration::millisecond );
}
