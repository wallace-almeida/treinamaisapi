package com.adontoApi.spec;

import java.util.ArrayList;
import java.util.List;

import com.adontoApi.entity.User;
import com.adontoApi.filter.SupIntegracaoFilter;
import jakarta.persistence.criteria.Predicate;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.util.StringUtils;



public class UserSpec {
/*
	public static Specification<User> withFilter(SupIntegracaoFilter filter) {

		return (root, query, builder) -> {
			List<Predicate> predicates = new ArrayList<Predicate>();

			
			if (filter.getIdSuprimento() != null) {
				 predicates.add(builder.equal(root.get("idSuprimento"), filter.getIdSuprimento()));
			}
			
			if (StringUtils.hasText(filter.getTpOperacao() )) {
				predicates.add(builder.like(builder.trim(builder.upper(root.get("tpOperacao"))), filter.getTpOperacao()));
			}
			
			if (StringUtils.hasText(filter.getStOperacao() )) {
				predicates.add(builder.like(builder.trim(builder.upper(root.get("stOperacao"))), filter.getStOperacao()));
			}
			
		
			
			
			
			return builder.and(predicates.toArray(new Predicate[0]));
		};

	}
*/
}
