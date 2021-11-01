/***************************************************************************
  reostableview.h - ReosTableView

 ---------------------
 begin                : 26.10.2021
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
#ifndef REOSTABLEVIEW_H
#define REOSTABLEVIEW_H

#include <QTableView>
#include <QHeaderView>

class ReosTimeSerieModel;

class ReosHorizontalHeaderView: public QHeaderView
{
  public:
    ReosHorizontalHeaderView( QWidget *parent = nullptr );

  protected:
    QSize sectionSizeFromContents( int logicalIndex ) const override;
};

class ReosTimeSerieTableView : public QTableView
{
    Q_OBJECT
  public:
    ReosTimeSerieTableView( QWidget *parent );
    void setModel( QAbstractItemModel *model ) override;

  protected:
    void keyPressEvent( QKeyEvent *event ) override;
    void contextMenuEvent( QContextMenuEvent *event ) override;

  private:
    QList<QVariantList> clipBoardToVariantList();
    ReosTimeSerieModel *timeSerieModel() const;

};

#endif // REOSTABLEVIEW_H
