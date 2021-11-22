/***************************************************************************
  reosplotitemlist.h

 ---------------------
 begin                : 19.11.2021
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
#ifndef REOSPLOTITEMLIST_H
#define REOSPLOTITEMLIST_H

#include <QWidgetAction>
#include <QAbstractListModel>
#include <QToolButton>
#include <QStyledItemDelegate>
#include <QListView>

class ReosPlotItem;
class ReosPlotWidget;
class ReosDataObject;
class ReosTimeSerieVariableTimeStep;


class ReosPlotItemListModel : public QAbstractListModel
{
  public:
    ReosPlotItemListModel( ReosPlotWidget *mPlotWidget, QObject *parent = nullptr );

    QModelIndex index( int row, int column, const QModelIndex &parent ) const override;
    QModelIndex parent( const QModelIndex &child ) const override;
    int rowCount( const QModelIndex &parent ) const override;
    int columnCount( const QModelIndex &parent ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;
    bool setData( const QModelIndex &index, const QVariant &value, int role ) override;
    Qt::ItemFlags flags( const QModelIndex &index ) const override;

    ReosPlotItem *addData( ReosTimeSerieVariableTimeStep *data );
    void clear();

    bool globalVisibilty() const;
    void setGlobalVisibilty( bool globalVisibilty );

  private:
    QList < std::tuple<ReosPlotItem *, ReosTimeSerieVariableTimeStep *, bool>> mPlot;
    bool mGlobalVisibilty = false;
    ReosPlotWidget *mPlotWidget = nullptr;
};

class ReosVariableTimeStepPlotListView : public QListView
{
  public:
    ReosVariableTimeStepPlotListView( ReosPlotWidget *plotWidget, QWidget *parent = nullptr );
    ReosPlotItem *addData( ReosTimeSerieVariableTimeStep *data );

    void clear();

    QSize sizeHint() const override;

  public slots:
    void setGlobalVisibility( bool isItemVisible );

  protected:
    void contextMenuEvent( QContextMenuEvent *event ) override;

  private:
    ReosPlotItemListModel *mModel;

};


class ReosVariableTimeStepPlotListButton: public QToolButton
{
  public:
    ReosVariableTimeStepPlotListButton( const QString &title, ReosPlotWidget *parent );

    ReosPlotItem *addData( ReosTimeSerieVariableTimeStep *data );
    void clear();

  private:
    ReosVariableTimeStepPlotListView *mView = nullptr;
};

#endif // REOSPLOTITEMLIST_H
