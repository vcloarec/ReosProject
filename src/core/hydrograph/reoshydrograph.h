/***************************************************************************
  reoshydrograph.h

 ---------------------
 begin                : 19.5.2021
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
#ifndef REOSHYDROGRAPH_H
#define REOSHYDROGRAPH_H

#include <QColor>
#include "reostimeserie.h"

//! Class that represents a hydrograph
class REOSCORE_EXPORT ReosHydrograph : public ReosTimeSerieVariableTimeStep
{
    Q_OBJECT
  public:
    ReosHydrograph( QObject *parent = nullptr, const QString &providerKey = QString(), const QString &dataSource = QString() )
      : ReosTimeSerieVariableTimeStep( parent, providerKey, dataSource ) {}

    QString type() const override {return QStringLiteral( "hydrograph" );}
    QColor color() const override;
    void setColor( const QColor &color );

    ReosEncodedElement encode() const;
    static ReosHydrograph *decode( const ReosEncodedElement &element, QObject *parent = nullptr );

  protected:
    ReosHydrograph( const ReosEncodedElement &element, QObject *parent = nullptr );

  private:
    QColor mColor;
};


class REOSCORE_EXPORT ReosHydrographStore : public ReosDataObject
{
    Q_OBJECT
  public:
    ReosHydrographStore( QObject *parent = nullptr ): ReosDataObject( parent ) {}

    //! Add an hydrograph to the sore, take ownership
    void addHydrograph( ReosHydrograph *hydrograph );

    //! Remove and destroy the hydrograph at position \a index
    void removeHydrograph( int index );

    int hydrographCount() const;

    //! Returns the list of the hydrograph names
    QStringList hydrographNames() const;

    //! Returns a pointer to the hydrograph at position \a index, nullptr if not exists
    ReosHydrograph *hydrograph( int index ) const;

    ReosEncodedElement encode() const;
    void decode( const ReosEncodedElement &element );

    QString type() const override {return QStringLiteral( "hydrograph-store" );}
  private:
    QList<ReosHydrograph *>  mHydrographs;

};

#endif // REOSHYDROGRAPH_H
