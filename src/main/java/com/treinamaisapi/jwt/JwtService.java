package com.treinamaisapi.jwt;

import java.security.Key;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

import org.springframework.stereotype.Service;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;

@Service
public class JwtService {

    private static final String SECRET_KEY = "614E645267556B58703273357638782F413F4428472B4B6250655368566D5971";
    private static final long ACCESS_TOKEN_VALIDITY = 1000L * 60 * 60; // 15 minutos
    private static final long REFRESH_TOKEN_VALIDITY = 1000L * 60 * 60 * 24 * 7; // 7 dias

    // =======================
    // Geração de tokens
    // =======================
    public String generateAccessToken(String username) {
        return generateToken(new HashMap<>(), username, ACCESS_TOKEN_VALIDITY, "ACCESS");
    }

    public String generateRefreshToken(String username) {
        return generateToken(new HashMap<>(), username, REFRESH_TOKEN_VALIDITY, "REFRESH");
    }

    public String generateToken(Map<String, Object> extraClaims, String username, long expiration, String tokenType) {
        extraClaims.put("type", tokenType); // claim opcional para diferenciar token
        return Jwts.builder()
                .setClaims(extraClaims)
                .setSubject(username)
                .setIssuedAt(new Date(System.currentTimeMillis()))
                .setExpiration(new Date(System.currentTimeMillis() + expiration))
                .signWith(getSignInKey(), SignatureAlgorithm.HS256)
                .compact();
    }

    // =======================
    // Extração de claims
    // =======================
    public String extractUsername(String token) {
        return extractClaim(token, Claims::getSubject);
    }

    public <T> T extractClaim(String token, Function<Claims, T> claimsResolver) {
        final Claims claims = extractAllClaims(token);
        return claimsResolver.apply(claims);
    }

    private Claims extractAllClaims(String token) {
        return Jwts.parserBuilder()
                .setSigningKey(getSignInKey())
                .build()
                .parseClaimsJws(token)
                .getBody();
    }

    private Key getSignInKey() {
        byte[] keyBytes = Decoders.BASE64.decode(SECRET_KEY);
        return Keys.hmacShaKeyFor(keyBytes);
    }

    // =======================
    // Validação de tokens
    // =======================
    public boolean isTokenValid(String token, String username) {
        return (username.equals(extractUsername(token))) && !isTokenExpired(token);
    }

    public boolean isAccessToken(String token) {
        String type = extractClaim(token, claims -> (String) claims.get("type"));
        return "ACCESS".equals(type);
    }

    public boolean isRefreshToken(String token) {
        String type = extractClaim(token, claims -> (String) claims.get("type"));
        return "REFRESH".equals(type);
    }

    private boolean isTokenExpired(String token) {
        return extractExpiration(token).before(new Date());
    }

    private Date extractExpiration(String token) {
        return extractClaim(token, Claims::getExpiration);
    }

}
