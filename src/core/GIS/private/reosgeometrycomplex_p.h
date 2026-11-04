#ifndef REOSGEOMETRYCOMPLEX_P_H
#define REOSGEOMETRYCOMPLEX_P_H

#include <QString>

#include <qgspointxy.h>
#include <qgscoordinatetransform.h>
#include <qgsvectorlayer.h>

class ReosMapExtent;
class QgsFeatureIterator;
class QPainter;
class ReosSpatialPosition;

class ReosGeometryComplex_p
{
  public:
    virtual ~ReosGeometryComplex_p() = default;

  protected:
    std::unique_ptr<QgsVectorLayer> mVectorLayer;

    ReosGeometryComplex_p() = default;
    ReosGeometryComplex_p( const QString &type, const QString &wktCrs );

    QgsPointXY toLayerCoordinates( const ReosSpatialPosition &position ) const;
    static QgsPointXY transformCoordinates( const QPointF &position, const QgsCoordinateTransform &transform );
    static QgsPointXY transformCoordinates( const QgsPointXY &position, const QgsCoordinateTransform &transform );
    const QgsCoordinateTransform toLayerTransform( const QString &crs ) const;
    const QgsCoordinateTransform toDestinationTransform( const QString &destinationCrs ) const;
    ReosMapExtent extent( const QString &destinationCrs ) const;
    QgsRectangle layerZone( const ReosMapExtent &zone ) const;
    QString crs() const;
    void setLayerCrs( const QString &wktCrs ) const;

    void renderGeometry( const QgsMapSettings &mapSettings, QPainter *painter ) const;

    QgsFeatureIterator closeFeatures( const ReosMapExtent &zone, QgsRectangle &rect ) const;
    QgsFeatureIterator closeFeaturesInLayerCoordinate( const QgsRectangle &rectLayer ) const;

  private:
    mutable QMap<QString, QgsCoordinateTransform> mCacheToLayerTransform;
    mutable QMap<QString, QgsCoordinateTransform> mCacheToDestinationTransform;
};

#endif // REOSGEOMETRYCOMPLEX_P_H
