##
InstallMethod( Opposite,
               [ IsCapCategory and IsMonoidalCategory, IsString ],
               
  function( category, name )
    local opposite_category, prop;
    
    if not HasIsFinalized( category ) or not IsFinalized( category ) then
        Error( "Input category must be finalized to create opposite category" );
    fi;
    
    opposite_category := CreateCapCategory( name );
    
    SetWasCreatedAsOppositeCategory( opposite_category, true );
    
    SetOpposite( opposite_category, category );
    
    CAP_INTERNAL_INSTALL_OPPOSITE_ADDS_FROM_CATEGORY( opposite_category, category );
    
    if category!.predicate_logic then
        
        INSTALL_TODO_LIST_ENTRIES_FOR_OPPOSITE_CATEGORY( category );
        
    fi;
    
    for prop in
      [ IsSkeletalCategory,
        IsMonoidalCategory,
        IsBraidedMonoidalCategory,
        IsSymmetricMonoidalCategory ]
      do
        if Tester( prop )( category ) then
            Setter( prop )( opposite_category, prop( category ) );
        fi;
    od;
    
    for prop in
      [ [ IsClosedMonoidalCategory, IsCoclosedMonoidalCategory ],
        [ IsSymmetricClosedMonoidalCategory, IsSymmetricCoclosedMonoidalCategory ],
        [ IsCartesianCategory, IsCocartesianCategory ] ]
      do
        
        if Tester( prop[1] )( category ) then
            Setter( prop[2] )( opposite_category, prop[1]( category ) );
        fi;
        
        if Tester( prop[2] )( category ) then
            Setter( prop[1] )( opposite_category, prop[2]( category ) );
        fi;
        
    od;
    
    Finalize( opposite_category );
    
    return opposite_category;
    
end );

