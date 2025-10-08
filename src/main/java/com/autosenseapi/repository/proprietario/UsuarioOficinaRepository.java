package com.autosenseapi.repository.proprietario;



import com.autosenseapi.entity.usuarios.oficina.UsuarioOficina;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface UsuarioOficinaRepository extends JpaRepository<UsuarioOficina, Long>, JpaSpecificationExecutor<UsuarioOficina>,  PagingAndSortingRepository<UsuarioOficina, Long>{





}
