/***************************************************************************
  reospolylinesstructures.h - ReosPolylinesStructures

 ---------------------
 begin                : 10.1.2022
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
#ifndef REOSPOLYLINESSTRUCTURES_H
#define REOSPOLYLINESSTRUCTURES_H

#include <QPolygonF>

class ReosPolylinesStructure
{
  public:
    ReosPolylinesStructure( const QString &crs );

    //! Adds a \a polyline to the structure with an identifier \a id
    void addPolylines( const QPolygonF &polyline, const QString &id = QString() );

    //! Returns the polyline with identifier \a id
    QPolygonF polyline( const QString &id ) const;

  private:
    QString mCrs;
    QList<QPolygonF> mPolylines;
    QStringList mPolylinesId;
};

#endif // REOSPOLYLINESSTRUCTURES_H
