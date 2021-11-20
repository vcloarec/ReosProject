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

class QColorDialog;


class ReosPlotItemListModel : public QAbstractListModel
{
  public:
    ReosPlotItemListModel( QObject *parent = nullptr );

    QModelIndex index( int row, int column, const QModelIndex &parent ) const override;
    QModelIndex parent( const QModelIndex &child ) const override;
    int rowCount( const QModelIndex &parent ) const override;
    int columnCount( const QModelIndex &parent ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;
    bool setData( const QModelIndex &index, const QVariant &value, int role ) override;
    Qt::ItemFlags flags( const QModelIndex &index ) const override;

    void addPlotItem( ReosPlotItem *item );
    void clear();

    bool globalVisibilty() const;
    void setGlobalVisibilty( bool globalVisibilty );

  private:
    QList < QPair<ReosPlotItem *, bool>> mPlotItems;
    bool mGlobalVisibilty = false;
};

class ReosPlotItemListView : public QListView
{
  public:
    ReosPlotItemListView( QWidget *parent );

    QSize sizeHint() const override;

  protected:
    void contextMenuEvent( QContextMenuEvent *event ) override;
};


class ReosOptionalPlotItemWidgetAction : public QWidgetAction
{
    Q_OBJECT
  public:
    ReosOptionalPlotItemWidgetAction( ReosPlotWidget *parent );

    void addPlotItem( ReosPlotItem *plotItem );
    void clear();

    bool globalVisibilty() const;

  public slots:
    void setGlobalVisibilty( bool globalVisibilty );

  private:
    QListView *mPlotItemListView = nullptr;
    ReosPlotItemListModel *mPlotItemsModel = nullptr;
    ReosPlotWidget *mPlotWidget = nullptr;
};


class ReosColorWidgetAction : public QWidgetAction
{
    Q_OBJECT
  public:
    ReosColorWidgetAction( QObject *parent );
    void setColor( const QColor &color );

  signals:
    void colorChanged( const QColor &color );

  private:
    QColorDialog *mColorWidget = nullptr;

};
class ReosOptionalPlotItemButton: public QToolButton
{
  public:
    ReosOptionalPlotItemButton( const QString &title, ReosPlotWidget *parent );

    void addPlotItem( ReosPlotItem *plotItem );
    void clear();

  private:
    ReosOptionalPlotItemWidgetAction *mWidgetAction = nullptr;
};

#endif // REOSPLOTITEMLIST_H
