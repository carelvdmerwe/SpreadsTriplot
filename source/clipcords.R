function (numclassesin, bagcords_Win) 
{
    if (numclassesin==2)
    {
        class1 <-list(list(x=bagcords_Win[[1]][,1],y=bagcords_Win[[1]][,2]))
        class2 <- list(list(x=bagcords_Win[[2]][,1],y=bagcords_Win[[2]][,2]))
        o1n2 <- polyclip(class1,class2,op="intersection")
        final <- c(o1n2)
    }
    
    if (numclassesin==3)
    {
        class1 <-list(list(x=bagcords_Win[[1]][,1],y=bagcords_Win[[1]][,2]))
        class2 <- list(list(x=bagcords_Win[[2]][,1],y=bagcords_Win[[2]][,2]))
        o1n2 <- polyclip(class1,class2,op="intersection")
        class3 <- list(list(x=bagcords_Win[[3]][,1],y=bagcords_Win[[3]][,2]))
        o2n3 <- polyclip(class2,class3,op="intersection")
        o1n3 <- polyclip(class1,class3,op="intersection")
        final <- c(o1n2,o2n3,o1n3)
    }
    if (numclassesin==4)
    {
        class1 <-list(list(x=bagcords_Win[[1]][,1],y=bagcords_Win[[1]][,2]))
        class2 <- list(list(x=bagcords_Win[[2]][,1],y=bagcords_Win[[2]][,2]))
        o1n2 <- polyclip(class1,class2,op="intersection")
        class3 <- list(list(x=bagcords_Win[[3]][,1],y=bagcords_Win[[3]][,2]))
        o2n3 <- polyclip(class2,class3,op="intersection")
        o1n3 <- polyclip(class1,class3,op="intersection")
        class4 <- list(list(x=bagcords_Win[[4]][,1],y=bagcords_Win[[4]][,2]))
        o1n4 <- polyclip(class1,class4,op="intersection")
        o2n4 <- polyclip(class2,class4,op="intersection")
        o3n4 <- polyclip(class3,class4,op="intersection")
        final <- c(o1n2,o2n3,o1n3,o1n4,o2n4,o3n4)
    }
    
    return(final)
}
