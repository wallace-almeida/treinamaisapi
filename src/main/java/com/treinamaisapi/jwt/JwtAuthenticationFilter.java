package com.treinamaisapi.jwt;

import java.io.IOException;

import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.JwtException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
public class JwtAuthenticationFilter extends OncePerRequestFilter {

    private final JwtService jwtService;
    private final CustomUserDetailsService userDetailsService;

    @Override
    protected boolean shouldNotFilter(HttpServletRequest request) {
        String path = request.getRequestURI();
        return path.contains("/auth/login")
                || path.contains("/auth/refresh");
        // se quiser liberar reset de senha sem auth, pode adicionar:
        // || path.contains("/auth/solicitar")
        // || path.contains("/auth/confirmar-codigo")
        // || path.contains("/auth/redefinir");
    }

    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain
    ) throws ServletException, IOException {

        final String authHeader = request.getHeader("Authorization");

        if (authHeader == null || !authHeader.startsWith("Bearer ")) {
            filterChain.doFilter(request, response);
            return;
        }

        final String jwt = authHeader.substring(7);

        try {
            // garante que é um access token (e não refresh)
            if (!jwtService.isAccessToken(jwt)) {
                filterChain.doFilter(request, response);
                return;
            }

            String userEmail = jwtService.extractUsername(jwt);

            if (userEmail != null &&
                    SecurityContextHolder.getContext().getAuthentication() == null) {

                var usuario = userDetailsService.loadUserByUsername(userEmail);

                if (jwtService.isTokenValid(jwt, usuario.getUsername())) {

                    UsernamePasswordAuthenticationToken authToken =
                            new UsernamePasswordAuthenticationToken(
                                    usuario,
                                    null,
                                    usuario.getAuthorities()
                            );

                    authToken.setDetails(
                            new WebAuthenticationDetailsSource().buildDetails(request)
                    );

                    SecurityContextHolder.getContext().setAuthentication(authToken);
                }
            }

        } catch (ExpiredJwtException ex) {
            escreverErro(response, 401, "AUTH_ERROR", "Token expirado");
            return;
        } catch (JwtException ex) {
            // qualquer problema de assinatura, estrutura, etc.
            escreverErro(response, 401, "AUTH_ERROR", "Token inválido");
            return;
        }

        filterChain.doFilter(request, response);
    }

    private void escreverErro(HttpServletResponse response,
                              int status,
                              String error,
                              String message) throws IOException {
        response.setStatus(status);
        response.setContentType("application/json");
        response.setCharacterEncoding("UTF-8");
        response.getWriter().write("""
            {
              "status": %d,
              "error": "%s",
              "message": "%s"
            }
            """.formatted(status, error, message));
    }
}
