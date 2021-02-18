/***************************************************************************
  reostextfiledata.h - ReosTextFileData

 ---------------------
 begin                : 9.2.2021
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
#ifndef REOSTEXTFILEDATA_H
#define REOSTEXTFILEDATA_H

#include <QFile>
#include <QAbstractTableModel>
#include <QTextStream>
#include <QRegularExpression>

#include "reoscore.h"

class REOSCORE_EXPORT ReosTextFileData : public QAbstractTableModel
{
    Q_OBJECT
  public:
    ReosTextFileData( QObject *parent = nullptr );

    bool setFileName( const QString &fileName );
    void setLines( int headerLine, int firstDataline );
    int headerLine() const;
    int firstDataLine() const;
    int previewLineMaxCount() const;
    void setPreviewLineMaxCount( int previewLineCount );

    void setDelimiters( const QStringList &delimiters );
    QStringList delimiters() const;

    QStringList headers() const;

    QModelIndex index( int row, int column, const QModelIndex &parent ) const;
    QModelIndex parent( const QModelIndex &child ) const;
    int rowCount( const QModelIndex &parent ) const;
    int columnCount( const QModelIndex &parent ) const;
    QVariant data( const QModelIndex &index, int role ) const;
    QVariant headerData( int section, Qt::Orientation orientation, int role ) const;

    QVector<QString> columnValues( int columnIndex );

  signals:
    void headersChanged( const QStringList &headers );

  private:
    QString mFileName;
    QStringList mDelimiters;
    QRegularExpression mRegExpDelimiters;
    int mPreviewLineMaxCount = 25;
    int mHeaderLine = 0;
    int mFirstDataLine = 1;

    QStringList mHeaders;
    QStringList mPreviewData;

    bool parsePreview();
    QStringList splitLine( const QString &line ) const;

};

#endif // REOSTEXTFILEDATA_H
