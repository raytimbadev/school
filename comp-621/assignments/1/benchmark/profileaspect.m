aspect profileaspect

properties
    write_M_count = 0;
    access_M_count = 0;
    write_P_count = 0;
    access_P_count = 0;
    write_X_count = 0;
    access_X_count = 0;
end

methods
    function inc_access_M(this)
        this.access_M_count = this.access_M_count + 1;
    end

    function inc_write_M(this)
        this.write_M_count = this.write_M_count + 1;
    end

    function inc_access_P(this)
        this.access_P_count = this.access_P_count + 1;
    end

    function inc_write_P(this)
        this.write_P_count = this.write_P_count + 1;
    end

    function inc_access_X(this)
        this.access_X_count = this.access_X_count + 1;
    end

    function inc_write_X(this)
        this.write_X_count = this.write_X_count + 1;
    end
end

patterns
    accessP : get(P);
    accessX : get(X);
    accessM : get(M);
    writeP : set(P);
    writeX : set(P);
    writeM : set(M);
end

actions
    actAccessP : before accessP
        this.inc_access_P()
    end

    actAccessX : before accessX
        this.inc_access_X()
    end

    actAccessM : before accessM
        this.inc_access_M()
    end

    actWriteP : before writeP
        this.inc_write_P()
    end

    actWriteX : before writeX
        this.inc_write_X()
    end

    actWriteM : before writeM
        this.inc_write_M()
    end
end

end
