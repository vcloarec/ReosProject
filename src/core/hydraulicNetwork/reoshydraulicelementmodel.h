/***************************************************************************
  reoshydraulicelementmodel.h - ReosHydraulicElementModel

 ---------------------
 begin                : 30.12.2022
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
#ifndef REOSHYDRAULICELEMENTMODEL_H
#define REOSHYDRAULICELEMENTMODEL_H

#include <QAbstractListModel>

#include "reoscore.h"

class ReosHydraulicNetwork;
class ReosHydraulicNetworkElement;

class REOSCORE_EXPORT ReosHydraulicElementModel : public QAbstractListModel
{
  public:
    ReosHydraulicElementModel( ReosHydraulicNetwork *parent = nullptr );

    int rowCount( const QModelIndex &parent ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;

    QModelIndex elementToIndex( ReosHydraulicNetworkElement *element ) const;
    ReosHydraulicNetworkElement *indexToElement( const QModelIndex &index ) const;

  private slots:
    void updateElements();

  private:
    ReosHydraulicNetwork *mNetwork = nullptr;
    QList<ReosHydraulicNetworkElement *> mElements;
};

#endif // REOSHYDRAULICELEMENTMODEL_H
