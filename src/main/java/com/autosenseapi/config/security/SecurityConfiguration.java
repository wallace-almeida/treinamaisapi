package com.autosenseapi.config.security;


import com.autosenseapi.common.util.SSLUtils;
import com.autosenseapi.jwt.JwtAuthenticationFilter;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;



@Configuration
@EnableWebSecurity
public class SecurityConfiguration {

	
		@Autowired
	  private JwtAuthenticationFilter jwtAuthFilter;
	  
	@Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
        http
            .authorizeHttpRequests(authz -> authz
            	.requestMatchers("/api/**").permitAll() //se desejar liberar todos endpoints sem autenticacao
            	.requestMatchers("/auth/**").permitAll()
                .requestMatchers("/swagger-ui/**").permitAll()
                .requestMatchers("/v3/api-docs/**").permitAll()
                .requestMatchers("/v3/api-docs.yaml").permitAll()
                .requestMatchers("/swagger-ui.html").permitAll()
                .anyRequest().authenticated()
            )
            .addFilterBefore(jwtAuthFilter, UsernamePasswordAuthenticationFilter.class)  // Add this line
            .logout(logout -> logout.permitAll())
            .csrf().disable();  // Disable CSRF if it's not needed for the endpoints

        return http.build();
    }

	private void ignoreCertificates() {
		try {
			SSLUtils.turnOffSslChecking();
		} catch (Exception e) {
			throw new RuntimeException(e.getMessage());
		}
	}

	@Bean
	public PasswordEncoder passwordEncoder() {
		return new BCryptPasswordEncoder();
	}

}