#ifndef REOSHECRASPROJECT_H
#define REOSHECRASPROJECT_H

#include <QString>
#include <QMap>
#include <QPolygonF>
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
    ReosHecRasGeometry( const QString fileName );

    const QString &title() {return mTitle;}

    int area2dCount() const;
    QString area2dName( int i ) const;

    QList<BoundaryCondition> boundariesCondition( const QString &area2dName ) const;

  private:
    QString mFileName;
    QString mTitle;

    void parseGeometryFile();
    void parseStorageArea( QTextStream &stream, const QString storageName );
    void parseBoundaryCondition( QTextStream &stream, const QString &bcName );

    QList<FlowArea2D> mAreas2D;
    QMap<QString, QList<BoundaryCondition>> mBoundariesConditions;

};

class ReosHecRasProject
{
  public:
    ReosHecRasProject( const QString &projectFileName );

    int GeometriesCount() const;

    QStringList geometryIds() const;
    ReosHecRasGeometry geometry( const QString &id ) const;

  private:
    QString mFileName;
    QMap<QString, ReosHecRasGeometry> mGeometries;

    void parseProjectFile();

};

#endif // REOSHECRASPROJECT_H
