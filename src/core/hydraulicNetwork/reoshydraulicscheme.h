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
class ReosCalculationContext;

/**
 * \brief ReosHydraulicScheme is a clas that represent a scheme of hydraulic simulation.
 * An instance of this class contain:
 * - reference to the meteorological model used for watershed hydrograph calculation
 * - start and end time of simulation
 */

class REOSCORE_EXPORT ReosHydraulicScheme : public ReosDataObject
{
    Q_OBJECT
  public:
    ReosHydraulicScheme( ReosHydraulicSchemeCollection *collection = nullptr ) SIP_SKIP;

    ReosParameterString *schemeName() const SIP_SKIP;

    ReosEncodedElement encode() const SIP_SKIP;
    static ReosHydraulicScheme *decode( const ReosEncodedElement &element, ReosHydraulicSchemeCollection *collection, const ReosHydraulicNetworkContext &context ) SIP_SKIP;

    ReosMeteorologicModel *meteoModel() const SIP_SKIP;
    void setMeteoModel( ReosMeteorologicModel *meteoModel ) SIP_SKIP;

    //! Saves the configuration of element \a elementid contained in \a encodedelement
    void saveElementConfig( const QString &elementId, const ReosEncodedElement &encodedElement ) SIP_SKIP;

    //! Restores the encoded configuration of the element \a elementId, if not exist, creates empty one for the element
    ReosEncodedElement restoreElementConfig( const QString &elementId ) SIP_SKIP;

    QString type() const override SIP_SKIP {return staticType();}
    static QString staticType() SIP_SKIP {return QStringLiteral( "scheme" );}

    ReosCalculationContext calculationContext() const;

  signals:
    void dirtied();
    void meteoTimeWindowChanged();

  private:
    ReosHydraulicScheme( const ReosEncodedElement &element, ReosHydraulicSchemeCollection *collection, const ReosHydraulicNetworkContext &context );

    QPointer<ReosMeteorologicModel> mMeteoModel;
    ReosParameterString *mSchemeName = nullptr;

    QHash<QString, QByteArray> mElementsConfig;

};

#ifndef SIP_RUN
class REOSCORE_EXPORT ReosHydraulicSchemeCollection : public QAbstractListModel
{
    Q_OBJECT
  public:
    ReosHydraulicSchemeCollection( QObject *parent = nullptr );

    QModelIndex index( int row, int column, const QModelIndex &parent ) const;
    QModelIndex parent( const QModelIndex &child ) const;
    int rowCount( const QModelIndex &parent ) const;
    int columnCount( const QModelIndex &parent ) const;
    QVariant data( const QModelIndex &index, int role ) const;

    void addScheme( ReosHydraulicScheme *scheme );
    void removeScheme( int index );

    //! Removes all schemes present in the collection and create only one associated with the \a meteomodel
    void reset( ReosMeteorologicModel *meteoModel );

    //! Removes all schemes present in the collection
    void clear();

    //!
    void excludeScheme( ReosHydraulicScheme *scheme );

    int schemeCount() const;

    ReosHydraulicScheme *scheme( int index ) const;

    ReosHydraulicScheme *scheme( const QString &schemeId ) const;

    ReosHydraulicScheme *schemeByName( const QString &name ) const;

    int schemeIndex( const QString &schemeId ) const;

    ReosEncodedElement encode() const;

    void decode( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context );

  signals:
    void dirtied();

  private:
    QList<ReosHydraulicScheme *> mHydraulicSchemes;
};
#endif //SIP no run

#endif // REOSHYDRAULICSCHEME_H
