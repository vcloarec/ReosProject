#ifndef REOSVERTEXZSPECIFIERWIDGET_H
#define REOSVERTEXZSPECIFIERWIDGET_H

#include <QWidget>
#include <QLineEdit>

#include "../ReosMesh/reosvertexzspecifier.h"
#include "../../Reos/Form/reosform.h"

class ReosVertexZSpecifierEntryWidgetModel;


namespace Ui {
class ReosVertexZSpecifierWidget;
}

class ReosVertexZSpecifierEntryWidget: public QWidget
{
public:
    ReosVertexZSpecifierEntryWidget(QWidget *parent);
    virtual ~ReosVertexZSpecifierEntryWidget();

    virtual QIcon icon() const=0;

    virtual ReosVertexZSpecifierFactory& factory() =0;

};

class ReosVertexZSpecifierSimpleValueWidget: public ReosVertexZSpecifierEntryWidget
{
    Q_OBJECT
public:
    ReosVertexZSpecifierSimpleValueWidget(QWidget *parent):ReosVertexZSpecifierEntryWidget(parent)
    {
        valueForm=new ReosForm(this);
        zValueParameterForm=new ReosFormParameterSimpleDouble(tr("Z value : "),0,nullptr," m");
        valueForm->addParamater(zValueParameterForm);
        setLayout(new QHBoxLayout);
        layout()->addWidget(valueForm->getWidget());

        connect(zValueParameterForm,&ReosFormParameterSimpleDouble::valueEdited,this,&ReosVertexZSpecifierSimpleValueWidget::ZValueHasBeenEditedChanged);
    }

    QIcon icon() const
    {
        return QIcon(QPixmap("://toolbar/ZSpecifierSimpleValue.png"));
    }

    ReosVertexZSpecifierFactory& factory() {return mFactory;}

private slots:
    void ZValueHasBeenEditedChanged()
    {
        mFactory.setZValue(zValueParameterForm->getValue());
    }
private:
    ReosVertexZSpecifierSimpleFactory mFactory;
    ReosForm *valueForm;
    ReosFormParameterSimpleDouble *zValueParameterForm;
};

class ReosVertexZSpecifierSlopeWidget: public ReosVertexZSpecifierEntryWidget
{
public:
    ReosVertexZSpecifierSlopeWidget(QWidget *parent):ReosVertexZSpecifierEntryWidget(parent)
    {
        valueForm=new ReosForm(this);
    valueForm->setOrientation(Qt::Horizontal);
        slopeParameterForm=new ReosFormParameterSimpleDouble(tr("Slope : "),0,nullptr," %");
        new ReosFormAction(valueForm,"Coucou");
        valueForm->addSeparator();
        valueForm->addParamater(slopeParameterForm);
        setLayout(new QHBoxLayout);
        layout()->addWidget(valueForm->getWidget());
    }

    QIcon icon() const
    {
        return QIcon(QPixmap("://toolbar/ZSpecifierVertexAndSlope.png"));
    }

    ReosVertexZSpecifierFactory& factory() {return mFactory;}
private:
    ReosVertexZSpecifierSimpleFactory mFactory;
    ReosForm *valueForm;
    ReosFormParameterSimpleDouble *slopeParameterForm;
};



class ReosVertexZSpecifierWidget : public QWidget
{
    Q_OBJECT

public:
    explicit ReosVertexZSpecifierWidget(QWidget *parent = nullptr);
    ~ReosVertexZSpecifierWidget();

    void assignZSpecifier(VertexPointer vert)
    {
        if(mCurrentEntryWidget)
            vert->setZSpecifier(mCurrentEntryWidget->factory());
    }

private slots:
    void listViewClicked(QModelIndex index);

private:

    void setCurrentZSpecifier(int i);
    void addEntry(ReosVertexZSpecifierEntryWidget* entry);

    Ui::ReosVertexZSpecifierWidget *ui;
    QList<ReosVertexZSpecifierEntryWidget*> mEntryWidgets;
    ReosVertexZSpecifierEntryWidgetModel *mEntriesModel;
    ReosVertexZSpecifierEntryWidget *mCurrentEntryWidget=nullptr;
};





class ReosVertexZSpecifierEntryWidgetModel:public QAbstractListModel
{
public:
    ReosVertexZSpecifierEntryWidgetModel(QList<ReosVertexZSpecifierEntryWidget*> &entriesList, QObject *parent=nullptr):QAbstractListModel(parent),
      mEntriesList(entriesList)
    {

    }

    // QAbstractItemModel interface
public:
    int rowCount(const QModelIndex &parent) const override
    {
        Q_UNUSED(parent);
        return mEntriesList.count();

    }
    QVariant data(const QModelIndex &index, int role) const override
    {
        if (role==Qt::DecorationRole)
            return mEntriesList.at(index.row())->icon();

        return QVariant();
    }

private:
    QList<ReosVertexZSpecifierEntryWidget*> &mEntriesList;

};

#endif // REOSVERTEXZSPECIFIERWIDGET_H
