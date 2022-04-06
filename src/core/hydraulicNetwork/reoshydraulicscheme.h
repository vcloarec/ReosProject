/***************************************************************************
  reoshydraulicscheme.h - ReosHydraulicScheme

 ---------------------
 begin                : 24.10.2021
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
#ifndef REOSHYDRAULICSCHEME_H
#define REOSHYDRAULICSCHEME_H

#include <QAbstractListModel>
#include <QPointer>

#include "reosdataobject.h"
#include "reosparameter.h"

class ReosParameterDateTime;
class ReosParameterString;
class ReosMeteorologicModel;
class ReosHydraulicSchemeCollection;
class ReosHydraulicNetworkContext;

/**
 * \brief ReosHydraulicScheme is a clas that represent a scheme of hydraulic simulation.
 * An instance of this class contain:
 * - reference to the meteorological model used for watershed hydrograph calculation
 * - start and end time of simulation
 */

class ReosHydraulicScheme : public ReosDataObject
{
  public:
    ReosHydraulicScheme( ReosHydraulicSchemeCollection *collection = nullptr );

    ReosParameterString *schemeName() const;
    ReosParameterDateTime *startTime() const;
    ReosParameterDateTime *endTime() const;

    ReosEncodedElement encode() const;
    static ReosHydraulicScheme *decode( const ReosEncodedElement &element, ReosHydraulicSchemeCollection *collection, const ReosHydraulicNetworkContext &context );

    ReosMeteorologicModel *meteoModel() const;
    void setMeteoModel( ReosMeteorologicModel *meteoModel );

    void saveElementConfig( const QString &elementId, const ReosEncodedElement &encodedElement );
    ReosEncodedElement restoreElementConfig( const QString &elementId );

  private:
    ReosHydraulicScheme( const ReosEncodedElement &element, ReosHydraulicSchemeCollection *collection, const ReosHydraulicNetworkContext &context );

    QPointer<ReosMeteorologicModel> mMeteoModel;
    ReosParameterString *mSchemeName = nullptr;
    ReosParameterDateTime *mStartTime = nullptr;
    ReosParameterDateTime *mEndTime = nullptr;

    QHash<QString, QByteArray> mElementsConfig;
};


class ReosHydraulicSchemeCollection : public QAbstractListModel
{
  public:
    ReosHydraulicSchemeCollection( QObject *parent = nullptr );

    QModelIndex index( int row, int column, const QModelIndex &parent ) const;
    QModelIndex parent( const QModelIndex &child ) const;
    int rowCount( const QModelIndex &parent ) const;
    int columnCount( const QModelIndex &parent ) const;
    QVariant data( const QModelIndex &index, int role ) const;

    void addScheme( ReosHydraulicScheme *scheme );
    void removeScheme( int index );
    void clear();
    int schemeCount() const;

    ReosHydraulicScheme *scheme( int index );

    ReosEncodedElement encode() const;
    void decode( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context );

  private:
    QList<ReosHydraulicScheme *> mHydraulicSchemes;
};


#endif // REOSHYDRAULICSCHEME_H
