package com.adontoApi.service;

import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.oauth2.jwt.Jwt;

public class UserInfo {

	public static Authentication getAuthentication() {
		return SecurityContextHolder.getContext().getAuthentication();
	}

	public static String getAuthUserID() {
		Jwt jwt = (Jwt) getAuthentication().getPrincipal();
		return jwt.getClaim("id");
	}

	public static Jwt getPrincipal() {
		return (Jwt) getAuthentication().getPrincipal();

	}

	public static String getCPF() {
		try {
		return getPrincipal().getClaimAsString("preferred_username");
		} catch (Exception e) {
			return "";
		}
	}

}
