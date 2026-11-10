/***************************************************************************
  reospolygonstructure_p.h - ReosPolygonsClassified_p

 ---------------------
 begin                : 5.2.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSPOLYGONSTRUCTURE_P_H
#define REOSPOLYGONSTRUCTURE_P_H

#define SIP_NO_FILE

#include <QVariantMap>

#include "reospolygonsclassified.h"
#include "reospolylinesstructure_p.h"

class QgsVectorLayer;
class QgsCategorizedSymbolRenderer;
class QgsSpatialIndex;

class ReosPolygonsClassifiedValues_p : public ReosPolygonsClassifiedValues
{
  public:
    double value( double x, double y, bool acceptClose = false ) const override;

    double defaultValue() const override;
    void setDefaultValue( double defVal ) override;

  private:
    mutable QgsGeometryEngine *mCacheGeom;
    mutable double mCacheValue;
    std::unique_ptr<QgsGeometryEngine> mZoneWithoutPolygon;
    std::unique_ptr<QgsSpatialIndex> mSpatialIndex;
    std::map<QgsFeatureId, std::unique_ptr<QgsGeometryEngine>> mGeomEngines;
    QHash<QgsFeatureId, double> mValues;
    QgsCoordinateTransform mTransform;
    double mTolerance = 0;
    double mDefaultValue = 0;

    friend class ReosPolygonsClassified_p;
};

class ReosPolygonsClassified_p : public ReosPolygonsClassified, private ReosGeometryComplex_p
{
    Q_OBJECT
  public:
    ReosPolygonsClassified_p() = default;
    ReosPolygonsClassified_p( const QString &wktCrs );
    ReosPolygonsClassified_p( const ReosEncodedElement &element );
    ~ReosPolygonsClassified_p();

    ReosPolygonsClassified *clone() const override;
    QObject *data() override;
    void addPolygon( const QPolygonF &polygon, const QString &classId, const QString &sourceCrs ) override;
    QStringList classes() const override;
    void addClass( const QString &classId, double value ) override;
    void removeClass( const QString &classId ) override;
    QString valueToClass( double value ) const override;
    ReosMapExtent extent( const QString &crs ) const override;
    QColor color( const QString &classId ) const override;
    double value( const QString &classId ) const override;
    int polygonsCount() const override;

    ReosGeometryStructureVertex *searchForVertex( const ReosMapExtent &zone ) const;

    ReosPolygonsClassifiedValues *values( const QString &destinationCrs ) const override;

    QUndoStack *undoStack() const override;

    ReosEncodedElement encode() const override;

    QString crs() const override;

    void render( void *mapSettings, QPainter *painter, bool highlight, const QPointF &highlightPosition ) const;

  private:
    QVariantMap mClasses;
    QgsCategorizedSymbolRenderer *mRenderer = nullptr;
    double mTolerance = 0.01;
    int mLastColorIndex = -1;

    mutable bool mDirty = true;

    QColor symbolColor( QgsSymbol *sym ) const;

    void addClassColor( const QString &classId, const QColor &color );
    void removeClassColor( const QString &classId );

    void init();

    void prepare( const QString &destinationCrs ) const;

    VertexS searchForVertexPrivate( QgsFeatureIterator &it, const QgsRectangle &rect ) const;

    friend class ReosPolygonStructureUndoCommandAddClass;
    friend class ReosPolygonStructureUndoCommandRemoveClass;
};


class ReosPolygonStructureUndoCommandAddClass : public QUndoCommand
{
  public:
    ReosPolygonStructureUndoCommandAddClass( ReosPolygonsClassified_p *structure, const QString &classId, double value, const QColor &color );

    void redo() override;
    void undo() override;

  private:
    ReosPolygonsClassified_p *mStructure = nullptr;
    QString mClassId;
    double mValue;
    QColor mColor;
};

class ReosPolygonStructureUndoCommandRemoveClass : public QUndoCommand
{
  public:
    ReosPolygonStructureUndoCommandRemoveClass( ReosPolygonsClassified_p *structure, const QString &classId );

    void redo() override;
    void undo() override;

  private:
    ReosPolygonsClassified_p *mStructure = nullptr;
    QString mClassId;
    double mValue;
    QColor mColor;
};

#endif // REOSPOLYGONSTRUCTURE_P_H
