#include "reosdialogbox.h"

ReosDialogBox::ReosDialogBox(QWidget *w, QWidget *parent):QDialog(parent),mLayout(new QVBoxLayout())
{
    setLayout(mLayout);
    QDialogButtonBox *buttonBox=new QDialogButtonBox(QDialogButtonBox::Ok|QDialogButtonBox::Cancel,this);
    if (w)
        mLayout->addWidget(w);


    mLayout->addWidget(buttonBox);

    setSizePolicy(QSizePolicy::Maximum,QSizePolicy::Maximum);

    connect(buttonBox,SIGNAL(accepted()),this,SLOT(accept()));
    connect(buttonBox,SIGNAL(rejected()),this,SLOT(reject()));
}
