/***************************************************************************
  reosvortexioprovider.h - ReosVortexIoProvider

 ---------------------
 begin                : 13.6.2024
 copyright            : (C) 2024 by vorteX-io
 email                : v.cloarec at vortex-io dot fr
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSVORTEXIOPROVIDER_H
#define REOSVORTEXIOPROVIDER_H

#include "reostimeseriesprovider.h"

class QNetworkAccessManager;
class QNetworkReply;

class ReosVortexIoProvider : public ReosTimeSerieVariableTimeStepProvider
{
  public:
    ReosVortexIoProvider();

    QString key() const override;
    QStringList fileSuffixes() const override;

    void load() override;

    QDateTime referenceTime() const override;
    QString valueUnit() const  override {return QString();}
    int valueCount() const override;
    double value( int i ) const override;
    double firstValue() const override;
    double lastValue() const override;
    double *data() override;
    const QVector<double> &constData() const override;
    ReosEncodedElement encode( const ReosEncodeContext &context ) const override;
    void decode( const ReosEncodedElement &element, const ReosEncodeContext &context ) override;
    ReosDuration relativeTimeAt( int i ) const override;
    ReosDuration lastRelativeTime() const override;
    const QVector<ReosDuration> &constTimeData() const override;
    bool canReadUri( const QString &uri ) const override;

    static  QVariantMap decodeUri( const QString &uri, bool &ok ) ;

    static QString staticKey();
    static QString dataType();

  private:
    QNetworkAccessManager *mNetworkManager = nullptr;
    QString mBaseUri;
    QDateTime mReferenceTime;
    QVector<ReosDuration> mTimes;
    QVector<double> mValues;

};


class ReosVortexIoProviderFactory : public ReosDataProviderFactory
{
  public:
    ReosDataProvider *createProvider( const QString &dataType ) const;
    QString key() const;
    bool supportType( const QString &dataType ) const;
    QVariantMap uriParameters( const QString &dataType ) const;
    QString buildUri( const QString &dataType, const QVariantMap &parameters, bool &ok ) const;


  private:
    ReosDataProvider::Capabilities mCapabilities = {ReosDataProvider::File};
};

#endif // REOSVORTEXIOPROVIDER_H
