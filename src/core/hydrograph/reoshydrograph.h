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
    ReosHydrograph( QObject *parent = nullptr ): ReosTimeSerieVariableTimeStep( parent ) {}

    QString type() const override {return QStringLiteral( "hydrograph" );}
    QColor color() const override;
    void setColor( const QColor &color );

  private:
    QColor mColor;
};


class REOSCORE_EXPORT ReosHydrographStore : public ReosDataObject
{
    Q_OBJECT
  public:
    ReosHydrographStore( QObject *parent = nullptr ): ReosDataObject( parent ) {}

    //! Add an hydrograph to the sore, take ownership
    void addHydrograph( ReosHydrograph *hydrograph )
    {
      hydrograph->setParent( this );
      mHydrographs.append( hydrograph );

      emit dataChanged();
    }

    //! Remove and destroy the hydrograph at position \a index
    void removeHydrograph( int index )
    {
      delete mHydrographs.takeAt( index );
      emit dataChanged();
    }

    int hydrographCount() const
    {
      return mHydrographs.count();
    }

    //! Returns the list of the hydrograph names
    QStringList hydrographNames() const
    {
      QStringList ret;
      for ( const ReosHydrograph *hyd : mHydrographs )
        ret.append( hyd->name() );

      return ret;
    }

    //! Returns a pointer to the hydrograph at position \a index, nullptr if not exists
    ReosHydrograph *hydrograph( int index ) const
    {
      if ( index >= 0 && index < mHydrographs.count() )
        return mHydrographs.at( index );

      return nullptr;
    }


    QString type() const override {return QStringLiteral( "hydrograph-store" );}
  private:
    QList<ReosHydrograph *>  mHydrographs;

};

#endif // REOSHYDROGRAPH_H
