/***************************************************************************
  reosmeshdatasetsource.h - ReosMeshDatasetSource

 ---------------------
 begin                : 3.4.2022
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
#ifndef REOSMESHDATASETSOURCE_H
#define REOSMESHDATASETSOURCE_H

#include <QString>
#include <QObject>

class ReosDuration;

class ReosMeshDatasetSource : public QObject
{
    Q_OBJECT
  public:
    ReosMeshDatasetSource( QObject *parent = nullptr );

    virtual int groupCount() const = 0;
    virtual int datasetCount( int groupIndex ) const = 0;
    virtual QString groupName( int groupIndex ) const = 0;
    virtual bool groupIsScalar( int groupIndex ) const = 0;
    virtual void groupMinMax( int groupIndex, double &minimum, double &maximum ) const = 0;
    virtual QDateTime groupReferenceTime( int groupIndex ) const = 0;
    virtual ReosDuration datasetRelativeTime( int groupIdex, int datasetIndex ) const = 0;
    virtual bool datasetIsValid( int groupIndex, int datasetIndex ) const = 0;
    virtual void datasetMinMax( int groupIndex, int datasetIndex, double &min, double &max ) const = 0;
    virtual QVector<double> datasetValues( int groupIndex, int index ) const = 0;

};

#endif // REOSMESHDATASETSOURCE_H
