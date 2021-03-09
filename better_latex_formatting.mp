_LatexSmallFractionConstant := 0:

# nicer latex formatting for a scalar
#  turns fractions a/b into \frac{a}{b} instead of
#  just "a/b", which is what the latex() command does
format_scalar := proc(scalar)
	if type(scalar, rational) and not type(scalar, integer) then
		if scalar > 0 then return sprintf("\\frac{%a}{%a}", numer(scalar), denom(scalar));
		elif scalar < 0 then return sprintf("-\\frac{%a}{%a}", numer((-1)*scalar), denom((-1)*scalar));
		else return "0";
		fi;
	fi:
	return latex(scalar, output=string);
end:

# nicer latex formatting for a monomial of the form c*x^n
# if <sign> is true, then we put a "+" or "-" before it as needed
# if <sign> is true and mono = 0, we return ""
# if <sign> is false and mono = 0, we return "0"
format_monomial := proc(mono, var, sign)
	local d, cf, ds, cfs, vs, signs;

	d := degree(mono, var);
	ds := cat("^{", convert(d,string), "}");
	cf := coeff(mono, var, d);
	cfs := format_scalar(cf);
	vs := convert(var,string);
	signs := "";

	if cf = 0 then
		if sign then return ""; else return "0"; fi;
	fi:
	if sign then
		if cf > 0 then signs := " + ";
		elif cf < 0 then signs := " - "; cf := (-1)*cf; cfs := format_scalar(cf); fi;
	fi:

	if d = 0 then ds := ""; vs := ""; fi:
	if d = 1 then ds := ""; fi:

	if cf = 1 and d > 0 then cfs := ""; fi;
	if cf = -1 and d > 0 then cfs := "-"; fi;

	return sprintf("%s%s%s%s", signs, cfs, vs, ds);
end:

# nicer latex formatting for univariate polynomials
# largest degree to smallest degree
# var = x, for example
format_polynomial_l2s := proc(poly, var)
	local p, deg, s, cf, mono, first, pow;

	p := expand(poly):
	deg := degree(p, var):

	first := true;
	for pow from deg to 0 by -1 do
		cf := coeff(p,var,pow):
		mono := cf * var^pow;

		if first then
			s := format_monomial(mono, var, false);
		else
			s := cat(s, format_monomial(mono, var, true)):
		fi:
		first := false;
	od:

	return s;
end:

# nicer latex formatting for univariate polynomials
# largest degree to smallest degree
# var = x, for example
format_polynomial_s2l := proc(poly, var)
	local p, deg, s, cf, mono, first, pow;

	p := expand(poly):
	deg := degree(p, var):

	s := "":

	first := true;
	for pow from 0 to deg do
		cf := coeff(p,var,pow):
		if cf = 0 then
			continue;
		fi:
		mono := cf * var^pow;

		if first then
			s := format_monomial(mono, var, false);
		else
			s := cat(s, format_monomial(mono, var, true)):
		fi:
		first := false;
	od:
	return s;
end:

# this is a helper function to take a polynomial and
# wrap it in parens if it contains more than one term
format_one_poly_term := proc(poly, var, l2s:=true)
	local poly_function;

	poly_function := `if`(l2s, format_polynomial_l2s, format_polynomial_s2l);

	if type(poly, `+`) then

		return sprintf("\\left(%s\\right)", poly_function(poly, var));
	else
		return sprintf("%s", poly_function(poly, var));
	fi:
end:

# nicer latex formatting for factored polynomials
#   optional parameter l2s=true means polys go from largest
#     to smallest degree, otherwise smallest to largest
# 	optional parameter outer_parents=true means we wrap
#     the answer in parens even if it doesn't factor
#     NOTE: we don't use outer parents around a negative sign
# if part_of_product is true, then we return "" instead of "1" and "-" instead of "-1"
format_poly_product := proc(poly, var, l2s:=true, sign:=false, part_of_product:=false)
	local poly_function, f, s, format_string, parts, scal, pp, pref, index;
	
	f := factor(poly);

	if type(f, `^`) then
		parts := [op(f)];

		return sprintf("%s^{%a}", format_one_poly_term(parts[1], var, l2s), parts[2]);
	fi:

	# note that type being "*" means poly won't be "1" or "-1" or any other scalar
	#   so we don't have to deal with that yet
	if type(f, `*`) then
		s := "";
		parts := [op(f)]:

		for index from 1 to nops(parts) do
			# first part might be a rational number multiplier
			# and could be positive or negative
			if index = 1 and type(parts[index], rational) then
				if sign then
					if parts[index] < 0 then
						if parts[index] = -1 then 
							scal := " - ";
						else
							scal := sprintf(" - %s",format_scalar((-1)*parts[index]));
						fi;
					else
						if parts[index] = 1 then
							scal := " + ";
						else
							scal := sprintf(" + %s",format_scalar(parts[index]));
						fi;
					fi;
				else
					if parts[index] = 1 then scal := "";
					elif parts[index] = -1 then scal := "-";
					else scal := sprintf("%s", format_scalar(parts[index]));
					fi;
				fi;

				s := cat(s, scal);
			else
				if type(parts[index], `^`) then
					pp := [op(parts[index])];
					s := cat(s, `if`(sign and index=1, " + ", ""), sprintf("%s^{%a}", format_one_poly_term(pp[1], var, l2s), pp[2]));
				else
					s := cat(s, `if`(sign and index=1, " + ", ""),  sprintf("%s", format_one_poly_term(parts[index], var, l2s)));
				fi;
			fi;
		od:
		return s;
	fi:

	# if we're here, then type isn't ^ or *. Could be a scalar, or a var with no power.
	if type(f, rational) then
		if sign then
			if poly = 1 then
				if part_of_product then return " + "; else return " + 1"; fi;
			elif poly = -1 then
				if part_of_product then return " - "; else return " - 1"; fi;
			elif poly > 0 then
				return sprintf(" + %s", format_scalar(poly));
			elif poly < 0 then
				return sprintf(" - %s", format_scalar((-1)*poly));
			else
				error "why should we have a 0????";
			fi;
		else
			if poly = 1 then
				if part_of_product then return ""; else return "1"; fi;
			elif poly = -1 then
				if part_of_product then return " -"; else return " -1"; fi;
			else
				return sprintf("%s", format_scalar(poly));
			fi;
		fi;
	else
		# I think now the only possibility is to be a polynomial
		#   that doesn't factor, like x^2 + x + 1 or something
		# We don't have to worry about sign because nothing factored
		# So we don't put it in parens unless it's part_of_product
		poly_function := `if`(l2s, format_polynomial_l2s, format_polynomial_s2l);
		pref := `if`(sign, " + ", "");
		if part_of_product then
			return cat(pref, format_one_poly_term(poly, var, l2s));
		else
			return cat(pref, poly_function(poly, var));
		fi;
	fi;
end:


# nicer latex formatting for minimal polynomials
#   (puts things in a sensible order)
# func = F, var = x, for example
format_minimal_polynomial := proc(min_poly, func, var)
	local mp, algdeg, term, s, func_string, cfs, cf, sign;

	mp := expand(min_poly);
	algdeg := degree(mp, func(var));
	s := "";

	sign := false;
	for term from algdeg to 0 by -1 do
		cf := expand(coeff(mp, func(var), term));
		
		if term = 0 then func_string := "";
		elif term = 1 then func_string := sprintf("%a",func(var));
		else func_string := sprintf("%a^{%a}",func(var),term);
		fi;

		if cf <> 0 then
			if coeff(cf, var, degree(cf, var)) > 0 then
				cfs := format_poly_product(cf, var, true, sign, `if`(term=0,false,true));
				s := cat(s, sprintf("%s%s", cfs, func_string));
			else
				cfs := format_poly_product((-1)*cf, var, true, false, true);
				s := cat(s, sprintf("-%s%s", cfs, func_string));
			fi;
		fi:
		sign := true;
	od:

	return cat(s, " = 0");
end:

remove_spaces := proc(s)
	return StringTools[SubstituteAll](s, "\\,", ""):
end:


# This is specific code for printing some types of generating functions.
format_some_gfs := proc(gf)
	if type(gf, ratpoly) then
		if denom(gf) = 1 then
			return format_polynomial_s2l(gf, x);
		else
			if tcoeff(numer(gf)) > 0 then
				return sprintf("\\frac{%s}{%s}", format_polynomial_s2l(numer(gf),x), format_polynomial_s2l(denom(gf),x));
			else
				return sprintf("\\frac{%s}{%s}", format_polynomial_s2l((-1)*numer(gf),x), format_polynomial_s2l((-1)*denom(gf),x));
			fi;
		fi;
	elif type(gf, radalgfun) and denom(gf) <> 1 then
		return sprintf("\\frac{%s}{%s}", remove_spaces(latex(numer(gf),output=string)), remove_spaces(latex(denom(gf),output=string)));
	else
		return remove_spaces(latex(gf, output=string));
	fi;
end:


# This overwrite's Maples internal processing of latex code for products.
# Most of the code in this function was written by Maplesoft, with just
#  small tweaks by me.
`latex/latex/*` := proc(e)
local subexp, den, ee, ff, subee, i, k, num, texlist, `\\,`, `\\frac `, `\\sqrt `, `{`, `}`, ccnt, keepcnt, ll, nlist, numTall, numShort, denTall, denShort;
global _LatexSmallFractionConstant;
option `Copyright (c) 1992 by the University of Waterloo. All rights reserved.`;
    if type(op(1, e), 'numeric') then
        num := op(1, e);
        if num < 0 then LaTeX:-CountCharacters(1); return `latex/latex/copy`('`-`'), `latex/print`(-e)
        elif not type(op(1, e), '`latex/istall`') then
            i := _LatexSmallFractionConstant;
            _LatexSmallFractionConstant := 0;
            ee := subsop(1 = 1, e);
            if type(ee, 'symbol') and type(num, 'fraction') and op(1, num) = 1 then texlist := `latex/print`(ee), "/", `latex/print`(1/num)
            else texlist := `latex/print`(num), `\\,`, `latex/print`(ee)
            end if;
            _LatexSmallFractionConstant := i;
            return texlist
        end if
    end if;
    texlist := NULL;
    numTall, numShort, denTall, denShort, ee := 1, 1, 1, 1, e;
    for subee in [op(ee)] do
        if type(subee, 'fraction') then num, den := op(1, subee), op(2, subee)
        elif type(subee, '`^`') and type(op(2, subee), 'rational') and op(2, subee) < 0 then num, den := 1, 1/subee
        else num, den := subee, 1
        end if;
        if type(num, '`latex/istall`') then numTall := numTall*num else numShort := numShort*num end if;
        if type(den, '`latex/istall`') then denTall := denTall*den else denShort := denShort*den end if
    end do;
    if denShort <> 1 or denTall <> 1 then
        if numTall <> 1 then texlist := `latex/print`(numTall); if type(numTall, '`+`') then texlist := ` \\left( `, texlist, ` \\right) ` end if else texlist := NULL end if;
        if denTall <> 1 then
            den := `if`(type(denTall, '`*`'), [op(denTall)], [denTall]);
            for subexp in den do nlist := `latex/print`(1/subexp); ll := LaTeX:-CheckLineBreak(); texlist := texlist, nlist, ll end do;
            texlist
        end if;
        if denShort <> 1 then texlist := '`{`', '`\\frac `', '`{`', `latex/print`(numShort), '`}{`', `latex/print`(denShort), '`}`', texlist, '`}`'
        elif type(numShort, '`+`') then texlist := '`{`', '`(`', `latex/print`(numShort), '`)`', texlist, '`}`'
        else texlist := '`{`', `latex/print`(numShort), texlist, '`}`'
        end if
    else
        i := 1;
        ff := [op(ee)];
        for k to nops(ff) do
            keepcnt := LaTeX:-ResetCount();
            if type(ff[k], {'`+`', 'series'}) then nlist := `latex/latex/prinpar`(ff[k])
            elif type(ff[k], 'complex'('numeric')) and nops(ff[k]) = 2 then nlist := `latex/latex/prinpar`(ff[k])
            elif i < nops(ee) and type(ff[k], 'specfunc'({Diff, diff})) then nlist := `latex/latex/prinpar`(ff[k])
            elif type(ff[k], 'specfunc'({`@`, `@@`})) then nlist := `latex/latex/prinpar`(ff[k])
            elif type(ff[k], 'And'('procedure', 'Not'('name'))) then nlist := `latex/latex/prinpar`(ff[k])
            else nlist := `latex/print`(ff[k])
            end if;
            if i < nops(ee) and
            (i = 1 and type(op(1, ee), 'integer') or type(ff[k], '`!`') or type(ff[k], {'string', 'symbol'}) and 1 < length(ff[k]) or has([`latex/print`(ff[k])], '`\\sqrt `'))
            then nlist := nlist, `\\,`
            end if;
            ccnt := LaTeX:-SetCount(keepcnt);
            ll := LaTeX:-CheckLineBreak(ccnt);
            texlist := texlist, nlist, ll;
            i := i + 1
        end do
    end if;
    texlist
end proc:

