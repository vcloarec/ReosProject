#include "reosdssutils.h"

extern "C" {
#include "heclib.h"
}


#include "reosduration.h"

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
