#ifndef REOSDSSWATCHER_H
#define REOSDSSWATCHER_H

#include <QObject>
#include <QThread>
#include <QTimer>

#include "reosdssfile.h"

class ReosDssWatcher : public QObject
{
    Q_OBJECT
  public:
    explicit ReosDssWatcher( const QString &filePath, const QList<ReosDssPath> &pathes )
      : mFilePath( filePath )
      , mPathes( pathes )
    {
      connect( &mTimer, &QTimer::timeout, this, &ReosDssWatcher::watch );
    }

    void startWatch()
    {
      mFile.reset( new ReosDssFile( mFilePath ) );
      if ( !mFile->isValid() )
        return;
      mTimer.start( mWatchInterval );
    }

    void setTimeWatchingTimeWindow( const ReosTimeWindow &newTimeWatchingTimeWindow );

  signals:
    void sendValues( const QString &path, const QDateTime &firstTime, const QList<double> &values, qint64 timeStepMillisec );

  private slots:
    void watch()
    {
      for ( const ReosDssPath &path : std::as_const( mPathes ) )
        watchForTimeSerie( path );
    }

  private:
    QString mFilePath;
    std::unique_ptr<ReosDssFile> mFile;
    ReosTimeWindow mTimeWatchingTimeWindow;
    QList<ReosDssPath> mPathes;
    QTimer mTimer;
    int mWatchInterval = 500;

    mutable QHash<QString, QDateTime> mLastObtainedTime;

    void watchForTimeSerie( const ReosDssPath &path )
    {
      QVector<double> values;
      ReosDuration timeStep;
      QDateTime startTime;
      mFile->getSeries( path, mTimeWatchingTimeWindow, values, timeStep, startTime );

      if ( values.count() > 0 )
      {
        const QDateTime lastTimeRecorded = mLastObtainedTime.value( path.string() );
        const QDateTime lastTimeValue = startTime.addMSecs( ( values.count() - 1 ) * timeStep.valueMilliSecond() );
        if ( !lastTimeRecorded.isValid() || lastTimeValue > lastTimeRecorded )
        {
          int timeStepCount;
          QDateTime firstTimeToSend;

          if ( lastTimeRecorded.isValid() )
          {
            timeStepCount = values.count();
            firstTimeToSend = startTime;
          }
          else
          {
            timeStepCount = static_cast<int>( lastTimeRecorded.msecsTo( lastTimeValue ) / timeStep.valueMilliSecond() );
            firstTimeToSend = lastTimeRecorded.addMSecs( timeStep.valueMilliSecond() );
          }

          mLastObtainedTime.insert( path.string(), lastTimeValue );

          QList<double> valuesToSend;
          valuesToSend.reserve( timeStepCount );
          for ( int i = values.count() - timeStepCount; i < values.count(); ++i )
            valuesToSend.append( values.at( i ) );

          emit sendValues( path.string(), firstTimeToSend, valuesToSend, timeStep.valueMilliSecond() );
        }
      }
    }

};

class ReosDssWatcherControler : public QObject
{
    Q_OBJECT
  public:
    explicit ReosDssWatcherControler( const QString &dssFilePath, QObject *parent )
      : QObject( parent )
      , mFilePath( dssFilePath )
    {}

    ~ReosDssWatcherControler()
    {
      mThread.quit();
      mThread.wait();
      mWatcher->deleteLater();
    }

    void addPathToWatch( const ReosDssPath &path )
    {
      mPathes.append( path );
    }

  public slots:
    void startWatching()
    {
      mWatcher = new ReosDssWatcher( mFilePath, mPathes );
      connect( mWatcher, &ReosDssWatcher::sendValues, this, &ReosDssWatcherControler::sendValues );
      connect( &mThread, &QThread::started, mWatcher, &ReosDssWatcher::startWatch );
      mWatcher->moveToThread( &mThread );
      mThread.start();
    }

  signals:
    void sendValues( const QString &path, const QDateTime &firstime, const QList<double> &values, qint64 timeStepMillisec );

  private:
    QThread mThread;
    QString mFilePath;
    QList<ReosDssPath> mPathes;
    ReosDssWatcher *mWatcher = nullptr;
};

#endif // REOSDSSWATCHER_H
