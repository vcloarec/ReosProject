#include "reosdataupdater.h"

#include <QTimer>

#include "reosdataobject.h"
#include "reostimeseries.h"

ReosDataUpdater::ReosDataUpdater( ReosDataObject *dataObject )
  : QObject( dataObject )
  , mTimer( new QTimer( this ) )
{
  connect( mTimer, &QTimer::timeout, this, &ReosDataUpdater::askUpdate );
}

void ReosDataUpdater::startWatching( int checkDelay )
{
  mTimer->start( checkDelay * 1000 );
}

void ReosDataUpdater::stopWatching()
{
  mTimer->stop();
}

ReosTimeSeriesUpdater::ReosTimeSeriesUpdater( ReosTimeSeries *timeSeries )
  : ReosDataUpdater( timeSeries )
  , mTimeSeries( timeSeries )
{
  connect( mTimeSeries->dataProvider(), &ReosDataProvider::loadingFinished, this, &ReosTimeSeriesUpdater::onDataCouldHaveChanged );
}

void ReosTimeSeriesUpdater::askUpdate()
{
  mTimeSeries->dataProvider()->load();
}

void ReosTimeSeriesUpdater::onDataCouldHaveChanged()
{
  QDateTime lastCurrentTimeStep = mTimeSeries->timeAt( mTimeSeries->valueCount() - 1 );
  if ( lastCurrentTimeStep != mLastNewTimeWindow.end() )
  {
    mLastNewTimeWindow = ReosTimeWindow( mLastNewTimeWindow.end(), lastCurrentTimeStep );
    emit dataUpdated();
  }
}

ReosTimeWindow ReosTimeSeriesUpdater::lastNewTimeWindow() const
{
    return mLastNewTimeWindow;
}
