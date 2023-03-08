/***************************************************************************
  reospolygonstructure_p.h - ReosPolygonStructure_p

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

#include "reospolygonstructure.h"
#include "reospolylinesstructure_p.h"

class QgsVectorLayer;
class QgsCategorizedSymbolRenderer;
class QgsSpatialIndex;

class ReosPolygonStructureValues_p : public ReosPolygonStructureValues
{
  public:
    double value( double x, double y, bool acceptClose = false ) const override;

    double defaultValue()  const override;
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

    friend class ReosPolygonStructure_p;
};

class ReosPolygonStructure_p : public ReosPolygonStructure, private ReosGeometryStructure_p
{
    Q_OBJECT
  public:
    ReosPolygonStructure_p() = default;
    ReosPolygonStructure_p( const QString &wktCrs );
    ReosPolygonStructure_p( const ReosEncodedElement &element );
    ~ReosPolygonStructure_p();

    ReosPolygonStructure *clone() const override;
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

    ReosPolygonStructureValues *values( const QString &destinationCrs ) const override;

    QUndoStack *undoStack() const override;

    ReosEncodedElement encode() const override;

    QString crs() const override;

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

    friend class ReosPolygonStructureUndoCommandAddClass;
    friend class ReosPolygonStructureUndoCommandRemoveClass;
};


class ReosPolygonStructureUndoCommandAddClass : public QUndoCommand
{
  public:
    ReosPolygonStructureUndoCommandAddClass( ReosPolygonStructure_p *structure, const QString &classId, double value, const QColor &color );

    void redo() override;
    void undo() override;

  private:
    ReosPolygonStructure_p *mStructure = nullptr;
    QString mClassId;
    double mValue;
    QColor mColor;
};

class ReosPolygonStructureUndoCommandRemoveClass : public QUndoCommand
{
  public:
    ReosPolygonStructureUndoCommandRemoveClass( ReosPolygonStructure_p *structure, const QString &classId );

    void redo() override;
    void undo() override;

  private:
    ReosPolygonStructure_p *mStructure = nullptr;
    QString mClassId;
    double mValue;
    QColor mColor;
};

#endif // REOSPOLYGONSTRUCTURE_P_H
