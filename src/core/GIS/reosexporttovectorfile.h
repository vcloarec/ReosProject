/***************************************************************************
  reosexporttovectorfile.h - ReosExportToVectorFile

 ---------------------
 begin                : 6.3.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSEXPORTTOVECTORFILE_H
#define REOSEXPORTTOVECTORFILE_H

#define SIP_NO_FILE

#include <QStringList>
#include <QVariant>
#include <memory>

#include "reoscore.h"

class QgsVectorFileWriter;
class QgsAbstractGeometry;

class REOSCORE_EXPORT ReosExportToVectorFile
{
  public:
    enum GeometryType
    {
      Polyline,
      Polygon
    };

    struct Field
    {
      QString name;
      QVariant::Type type;
      QString typeName;
      int length;
    };

    ReosExportToVectorFile( const QString &fileName,
                            const QList<Field> fields,
                            GeometryType geometryType,
                            const QString &crs );
    ~ReosExportToVectorFile();

    //! Adds a polygon to export, does nothing if the type of this instance is not a polygon type
    void addPolygon( const QPolygonF &polygon, const QVariantMap &attributes );

    //! Adds a polyline to export, does nothing if the type of this instance is not a polyline type
    void addPolyline( const QPolygonF &polygon, const QVariantMap &attributes );

  private:
    std::unique_ptr<QgsVectorFileWriter> mFileWriter;
    GeometryType mType;
    QList<Field> mFields;

    void addFeature( QgsAbstractGeometry *ageom, const QVariantMap &attributes );
};

#endif // REOSEXPORTTOVECTORFILE_H
