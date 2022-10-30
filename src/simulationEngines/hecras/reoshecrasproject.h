#ifndef REOSHECRASPROJECT_H
#define REOSHECRASPROJECT_H

#include <QString>
#include <QMap>
#include <QPolygonF>
#include <QDateTime>
#include <memory>

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

    static QDate hecRasDateToDate( const QString &hecrasDate );

  private:
    QString mFileName;
    QMap<QString, ReosHecRasGeometry> mGeometries;
    QMap<QString, ReosHecRasPlan> mPlans;
    QString mCurrentPlan;

    void parseProjectFile();

};

#endif // REOSHECRASPROJECT_H
