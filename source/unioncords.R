function (numclassesin,bagcords_Win) 
{
    if (numclassesin==2)
    {
        class1 <-list(list(x=bagcords_Win[[1]][,1],y=bagcords_Win[[1]][,2]))
        class2 <- list(list(x=bagcords_Win[[2]][,1],y=bagcords_Win[[2]][,2]))
        final <- polyclip(class1,class2,op="union")
    }
    
    if (numclassesin==3)
    {
        class1 <-list(list(x=bagcords_Win[[1]][,1],y=bagcords_Win[[1]][,2]))
        class2 <- list(list(x=bagcords_Win[[2]][,1],y=bagcords_Win[[2]][,2]))
        class3 <- list(list(x=bagcords_Win[[3]][,1],y=bagcords_Win[[3]][,2]))
        final <- polyclip(polyclip(class1,class2,op="union"),class3,op="union")
    }
    if (numclassesin==4)
    {
        class1 <-list(list(x=bagcords_Win[[1]][,1],y=bagcords_Win[[1]][,2]))
        class2 <- list(list(x=bagcords_Win[[2]][,1],y=bagcords_Win[[2]][,2]))
        class3 <- list(list(x=bagcords_Win[[3]][,1],y=bagcords_Win[[3]][,2]))
        class4 <- list(list(x=bagcords_Win[[4]][,1],y=bagcords_Win[[4]][,2]))
        final <- polyclip(polyclip(polyclip(class1,class2,op="union"),class3,op="union"),class4,op="union")
    }
    return(final)
}
