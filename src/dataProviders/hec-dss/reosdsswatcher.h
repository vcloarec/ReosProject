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
    ReosDssWatcher( const QString &filePath )
      : mFilePath( filePath )
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
    void sendValues( QString path, QList<double> values, qint64 timeStepMillisec );

  private slots:
    void watch()
    {

    }

  private:
    QString mFilePath;
    std::unique_ptr<ReosDssFile> mFile;
    ReosTimeWindow mTimeWatchingTimeWindow;
    QList<ReosDssPath> mPaths;
    QTimer mTimer;
    int mWatchInterval = 500;

    QHash<QString, QDateTime> mLastObtainedTime;

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
          if ( lastTimeRecorded.isValid() )
          {
            timeStepCount = values.count();
          }
          else
          {
            timeStepCount = lastTimeRecorded.msecsTo( lastTimeValue ) / timeStep.valueMilliSecond();
          }

          mLastObtainedTime.insert( path.string(), lastTimeValue );

          QList<double> valuesToSend;
          valuesToSend.reserve( timeStepCount );
          for ( int i = values.count() - timeStepCount; i < values.count(); ++i )
          {
            valuesToSend.append( values.at( i ) );
          }

          emit sendValues( path.string(), valuesToSend, timeStep.valueMilliSecond() );
        }
      }
    }

};

class ReosDssWatcherControler : public QObject
{
    Q_OBJECT
  public:
    ReosDssWatcherControler( const QString &dssFilePath )
      : mFilePath( dssFilePath )
    {

    }

    ~ReosDssWatcherControler()
    {
      mThread.quit();
      mThread.wait();
      mWatcher->deleteLater();
    }

    void startWatching()
    {
      mWatcher = new ReosDssWatcher( mFilePath );
      QObject::connect( &mThread, &QThread::started, mWatcher, &ReosDssWatcher::startWatch );
      mWatcher->moveToThread( &mThread );
    }

  private slots:
    void receiveFlowValues( QString &path, QList<double> values, qint64 timeStepMillisec )
    {

    }

  private:
    QThread mThread;
    QString mFilePath;
    ReosDssWatcher *mWatcher = nullptr;
};

#endif // REOSDSSWATCHER_H
