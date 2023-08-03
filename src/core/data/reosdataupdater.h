#ifndef REOSDATAUPDATER_H
#define REOSDATAUPDATER_H

#include <QObject>
#include <QDateTime>

#include "reoscore.h"
#include "reosduration.h"

class QTimer;

class ReosDataObject;
class ReosTimeSeries;

/**
 * The ReosDataUpdater class is an abstract class used to update regulary data object
 */
class REOSCORE_EXPORT ReosDataUpdater : public QObject SIP_ABSTRACT
{
    Q_OBJECT
  public:
    //! Constructor with \a dataObject as parent
    ReosDataUpdater( ReosDataObject *dataObject );

    //! Start watching every \a checkDelay seconds
    void startWatching( int checkDelay );

    //! Stop watching the data
    void stopWatching();

  public slots:
    //! Asks update of the data object
    virtual void askUpdate() = 0;

  signals:
    //! Emitted when the data object is updated
    void dataUpdated();

  private slots:
    virtual void onDataCouldHaveChanged() = 0;

  private:
    QTimer *mTimer = nullptr;
};


class REOSCORE_EXPORT ReosTimeSeriesUpdater : public ReosDataUpdater
{
    Q_OBJECT
  public:
    ReosTimeSeriesUpdater( ReosTimeSeries *timeSeries );

    void askUpdate() override;

    //! Returns the time windows which is new comparing to the former series
    ReosTimeWindow lastNewTimeWindow() const;

  private slots:
    void onDataCouldHaveChanged() override;

  private:
    ReosTimeSeries *mTimeSeries = nullptr;
    ReosTimeWindow mLastNewTimeWindow;

};

#endif // REOSDATAUPDATER_H
