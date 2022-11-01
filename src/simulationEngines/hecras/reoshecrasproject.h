#ifndef REOSHECRASPROJECT_H
#define REOSHECRASPROJECT_H

#include <QString>
#include <QMap>
#include <QPolygonF>
#include <QDateTime>
#include <memory>

#include "reosduration.h"

class QTextStream;


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
      QString name;
      QPointF middlePosition;
    };

    ReosHecRasGeometry() = default;
    ReosHecRasGeometry( const QString &fileName );

    const QString &title() const {return mTitle;}

    int area2dCount() const;
    QString area2dName( int i ) const;
    FlowArea2D area2d( int i ) const;

    QList<BoundaryCondition> boundariesConditions( const QString &area2dName ) const;
    QList<BoundaryCondition> allBoundariesConditions() const;

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

  private:
    QString mFileName;
    QString mTitle;
    QString mGeometryFile;
    QString mFlowFile;

    QDateTime mStartTime;
    QDateTime mEndTime;

    void parsePlanFile();
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
    };

    ReosHecRasFlow() = default;
    ReosHecRasFlow( const QString &fileName );

    const QString &title() const;

    int boundariesCount() const;
    const BoundaryFlow &boundary( int index ) const;

  private:
    QString mFileName;
    QString mTitle;

    void parseFlowFile();
    QString parseBoundary( QTextStream &stream, const QString &firstLine );
    QVector<double> parseValues( QTextStream &stream, const QString &firstLine );

    QList<BoundaryFlow> mBoundaries;

};

class ReosHecRasProject
{
  public:
    ReosHecRasProject( const QString &projectFileName );

    QString currentPlanId() const;
    QStringList planIds() const;
    QString planTitle( const QString &id ) const;

    ReosHecRasPlan plan( const QString &planId ) const;

    int GeometriesCount() const;

    QStringList geometryIds() const;
    ReosHecRasGeometry geometry( const QString &id ) const;
    ReosHecRasGeometry currentGeometry() const;

    QStringList flowIds() const;
    ReosHecRasFlow flow( const QString &id ) const;
    ReosHecRasFlow currentFlow() const;

    static QDate hecRasDateToDate( const QString &hecrasDate );

  private:
    QString mFileName;
    QMap<QString, ReosHecRasPlan> mPlans;
    QMap<QString, ReosHecRasGeometry> mGeometries;
    QMap<QString, ReosHecRasFlow> mFlows;
    QString mCurrentPlan;

    void parseProjectFile();

};

#endif // REOSHECRASPROJECT_H
