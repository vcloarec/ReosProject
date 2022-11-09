#ifndef REOSHECRASPROJECT_H
#define REOSHECRASPROJECT_H

#include <QString>
#include <QMap>
#include <QPolygonF>
#include <QDateTime>
#include <QDir>
#include <memory>

#include "reosduration.h"

class QTextStream;
class ReosHecRasSimulation;

class ReosHecRasGeometry
{
  public:
    struct FlowArea2D
    {
      QString name;
      QPolygonF surface;
    };

    struct BoundaryCondition
    {
      QString area;
      QString name;
      QPointF middlePosition;

      QString id() const
      {
        return area + '-' + name;
      }
    };

    ReosHecRasGeometry() = default;
    ReosHecRasGeometry( const QString &fileName );

    const QString &title() const {return mTitle;}

    int area2dCount() const;
    QString area2dName( int i ) const;
    FlowArea2D area2d( int i ) const;

    QList<BoundaryCondition> boundariesConditions( const QString &area2dName ) const;
    QList<BoundaryCondition> allBoundariesConditions() const;

    QString fileName() const;

  private:
    QString mFileName;
    QString mTitle;

    QList<FlowArea2D> mAreas2D;
    QMap<QString, QList<BoundaryCondition>> mBoundariesConditions;

    void parseGeometryFile();
    void parseStorageArea( QTextStream &stream, const QString storageName );
    void parseBoundaryCondition( QTextStream &stream, const QString &bcName );
};

class ReosHecRasPlan
{
  public:
    ReosHecRasPlan() = default;
    ReosHecRasPlan( const QString &fileName );

    QString geometryFile() const;
    QString flowFile() const;

    const QString &title() const;

    const QDateTime &startTime() const;
    const QDateTime &endTime() const;

    const ReosDuration computeInterval() const;
    const ReosDuration outputIntevall() const;
    const ReosDuration detailedOutputInteval() const;
    const ReosDuration mappingInteval() const;

    void changeSimulationTimeInFile( const QDateTime &startTime, const QDateTime &endTime, const ReosHecRasSimulation *simulation ) const;

    const QString &fileName() const;

    const QString &shortIdentifier() const;

    static ReosDuration computationIntervalStringToDuration( const QString &interval );
    static QString durationToComputationInterval( const ReosDuration &duration );

    static const QMap<ReosDuration, QString> &computationIntervals();

  private:
    QString mFileName;
    QString mTitle;
    QString mGeometryFile;
    QString mFlowFile;
    QString mShortIdentifier;

    QDateTime mStartTime;
    QDateTime mEndTime;

    ReosDuration mComputeInterval;
    ReosDuration mOutputInterval;
    ReosDuration mDetailedInterval;
    ReosDuration mMappingInterval;

    void parsePlanFile();

    static QMap<ReosDuration, QString> sIntervals;
};

class ReosHecRasFlow
{
  public:

    enum class Type
    {
      None,
      FlowHydrograph,
      StageHydrograph,
      NormalDepth
    };

    struct BoundaryFlow
    {
      QString area;
      QString boundaryConditionLine;
      Type type = Type::None;
      ReosDuration interval;
      QVector<double> values;
      bool isDss = false;
      QString dssFile;
      QString dssPath;

      QString id() const
      {
        return area + '-' + boundaryConditionLine;
      }
    };

    ReosHecRasFlow() = default;
    ReosHecRasFlow( const QString &fileName );

    const QString &title() const;

    int boundariesCount() const;
    const BoundaryFlow &boundary( int index ) const;
    BoundaryFlow boundary( const QString &area, const QString &boundaryLine, bool &found ) const;

    /**
     * Changes the Flow file to apply the flows condition in \a flows
     *
     * \note only flows condition defined in a DSS file are considered,
     * other will be ignored and related lines in the file will be unchanged
     */
    bool applyBoudaryFlow( const QList<BoundaryFlow> &flows );

  private:
    QString mFileName;
    QString mTitle;

    void parseFlowFile();
    QString parseBoundary( QTextStream &stream, const QString &firstLine );
    QVector<double> parseValues( QTextStream &stream, const QString &firstLine );
    bool parseLocation( const QString &locationLine, QString &area, QString &boundaryLine ) const;

    QList<BoundaryFlow> mBoundaries;

};

class ReosHecRasProject
{
  public:
    ReosHecRasProject( const QString &projectFileName );

    QString currentPlanId() const;
    QStringList planIds() const;
    QString planTitle( const QString &id ) const;
    QDir directory() const;

    ReosHecRasPlan currentPlan() const;
    ReosHecRasPlan plan( const QString &planId ) const;

    int GeometriesCount() const;

    QStringList geometryIds() const;
    ReosHecRasGeometry geometry( const QString &id ) const;
    ReosHecRasGeometry geometryFromPlan( const QString &planId ) const;
    QString currentGeometryFileName() const;
    ReosHecRasGeometry currentGeometry() const;

    QStringList flowIds() const;
    ReosHecRasFlow flow( const QString &id ) const;
    ReosHecRasFlow flowFromPlan( const QString &planId ) const;
    ReosHecRasFlow currentFlow() const;

    static QDate hecRasDateToDate( const QString &hecrasDate );
    static QString dateToHecRasDate( const QDate &date );

    const QString &fileName() const;

    const QString &projectName() const;

    const QString dssResultFile( const QString &planId ) const;

  private:
    QString mFileName;
    QMap<QString, ReosHecRasPlan> mPlans;
    QMap<QString, ReosHecRasGeometry> mGeometries;
    QMap<QString, ReosHecRasFlow> mFlows;
    QString mCurrentPlan;
    QString mProjectName;

    void parseProjectFile();

};

#endif // REOSHECRASPROJECT_H
