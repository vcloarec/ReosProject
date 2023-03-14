/***************************************************************************
  reosdataobject.h - ReosDataObject

 ---------------------
 begin                : 4.2.2021
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
#ifndef REOSDATAOBJECT_H
#define REOSDATAOBJECT_H

#include <QObject>

#include "reoscore.h"

class ReosEncodedElement;

//! Base class uses to be an interface for data
class REOSCORE_EXPORT ReosDataObject: public QObject
{
    Q_OBJECT
  public:
    ReosDataObject( QObject *parent = nullptr );

    //! Returns the type
    virtual QString type() const {return staticType();}

    //! Returns the name of the data object
    QString name() const SIP_SKIP;

    QString id() const;

    void encode( ReosEncodedElement &element ) const SIP_SKIP;
    void decode( const ReosEncodedElement &element ) SIP_SKIP;

    //! Static method that return the type of this class
    static QString staticType() SIP_SKIP {return QStringLiteral( "data" );}

  public slots:
    //! Sets the name of the data object
    void setName( const QString &name ) SIP_SKIP;
    virtual void updateData() const  SIP_SKIP {}; //TODO to set pure virtual

  signals:
    void dataChanged() const;
    void dataReset() const;
    void nameChanged( QString &name ) const SIP_SKIP;
    void isSetObsolete() const SIP_SKIP;
    void settingsChanged() const SIP_SKIP;

  protected:
    void registerUpstreamData( ReosDataObject *data ) SIP_SKIP;
    void deregisterUpstreamData( ReosDataObject *data ) SIP_SKIP;
    void setActualized() const SIP_SKIP;

    //! Return true whether the data need to be calculated or updated
    bool isObsolete() const SIP_SKIP;

  protected slots:
    void setObsolete();

  private:
    QString mName;
    QString mUid;
    mutable bool mIsObsolete = true;

//*** for tests
    friend class ReosRainfallTest;
    friend class ReosWatersehdTest;
};
#endif // REOSDATAOBJECT_H
