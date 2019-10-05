#include "reosvertexzspecifierwidget.h"
#include "ui_reosvertexzspecifierwidget.h"

ReosVertexZSpecifierWidget::ReosVertexZSpecifierWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::ReosVertexZSpecifierWidget)
{
    ui->setupUi(this);
    addEntry(new ReosVertexZSpecifierSimpleValueWidget(this));
    addEntry(new ReosVertexZSpecifierSlopeWidget(this));

    for (auto entry:mEntryWidgets)
        entry->hide();

    mEntriesModel= new ReosVertexZSpecifierEntryWidgetModel(mEntryWidgets,this);
    ui->mEntriesListView->setModel(mEntriesModel);
    setCurrentZSpecifier(0);

    connect(ui->mEntriesListView,&QListView::clicked,this,&ReosVertexZSpecifierWidget::listViewClicked);

}

ReosVertexZSpecifierWidget::~ReosVertexZSpecifierWidget()
{
    delete ui;
}

void ReosVertexZSpecifierWidget::listViewClicked(QModelIndex index)
{
    setCurrentZSpecifier(index.row());
}

void ReosVertexZSpecifierWidget::setCurrentZSpecifier(int i)
{
    for (auto entry:mEntryWidgets)
        entry->hide();
    ui->mEntriesListView->setCurrentIndex(mEntriesModel->index(i));
    mCurrentEntryWidget=mEntryWidgets.at(i);
    mCurrentEntryWidget->show();

}

void ReosVertexZSpecifierWidget::addEntry(ReosVertexZSpecifierEntryWidget *entry)
{
    mEntryWidgets.append(entry);
    ui->widgetSpecificZSpecifier->layout()->addWidget(entry);
}

ReosVertexZSpecifierEntryWidget::ReosVertexZSpecifierEntryWidget(QWidget *parent):QWidget(parent){}

ReosVertexZSpecifierEntryWidget::~ReosVertexZSpecifierEntryWidget() {}
